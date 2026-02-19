{-
    BNF Converter: TreeSitter Grammar Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Description   : This module converts BNFC grammar to the contents of a
                    tree-sitter grammar.js file

    Author        : Kangjing Huang (huangkangjing@gmail.com)
    Created       : 08 Nov, 2023

-}

module BNFC.Backend.TreeSitter.CFtoTreeSitter where

import BNFC.Abs (Reg)
import BNFC.Backend.TreeSitter.RegToJSReg
import BNFC.Backend.TreeSitter.MatchesEmpty(fixPointKnownEmpty, transformEmptyMatches, KnownEmpty, OptSym(..), OptSentForm, isKnownEmpty)
import BNFC.CF
import BNFC.Utils(when, applyWhen, cstring)
import BNFC.Lexing (mkLexer, LexType(..))
import BNFC.PrettyPrint

import Prelude hiding ((<>))

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.List.NonEmpty as List1

-- * Main entry point

-- | Create content of grammar.js file
cfToTreeSitter :: String -> Cat -> CF -> Doc
cfToTreeSitter name wordCat cf =
  -- Overall structure of grammar.js
  text "module.exports = grammar({"
    $+$ indent
      ( text "name:" <+> cstring name <> ","
          $+$ extrasSection
          $+$ wordSection
          $+$ rulesSection
      )
    $+$ text "});"
  where
    (commentTokens, lexTokens) =
      Either.partitionEithers $ Maybe.mapMaybe tokenFilter $ mkLexer cf

    tokenFilter (r, LexComment) = Just (Left r)
    tokenFilter (r, LexToken name) = Just (Right (r, name))
    tokenFilter (_, LexSymbols) = Nothing -- LexSymbols appear as literals within rule RHS

    extrasSection = prExtras commentTokens
    wordSection = prWord wordCat cf
    rulesSection =
      text "rules: {"
        $+$ indent
          ( prRules cf
              $+$ prTokenRules cf lexTokens
          )
        $+$ text "},"

-- * Functions to build parts of grammar.js

-- | Print rules for comments
prExtras :: [Reg] -> Doc
prExtras commentRegs =
  defineSymbol "extras" <> "["
    $+$ indent
      ( -- default rule for white spaces
        text "/\\s/,"
          $+$ vcat' commentDocs
      )
    $+$ text "],"
  where
    commentDocs = map (appendComma . text . printRegJSReg) commentRegs

-- | Print word section, this section is needed for tree-sitter
--   to do keyword extraction before any parsing/lexing, see
--   https://tree-sitter.github.io/tree-sitter/creating-parsers#keyword-extraction
--
--   This should be defined as a rule which matches a /superset/ of keywords
--   in the language. Usually, this would be some general identifier token. So,
--   this defaults to the built-in Ident token and can be specified by the user
--   with a command-line flag.
prWord :: Cat -> CF -> Doc
prWord wordCat cf =
  when (isUsedCat cf wordCat) $
    defineSymbol "word"
      <+> formatSent [NonOptional (Left wordCat)] <> ","

-- | Prints the rules in the grammar with the entry point first.
--
-- Since Treesitter requires a unique entry point, this will build a "virtual"
-- entry point which dispatches to each of the declared BNFC entry points via
-- a choice list. Additionally, the virtual entry point can match the empty string
-- (and is the only rule which can).
prRules :: CF -> Doc
prRules cf =
  prOneCat knownEmpty True virtEntryCat virtEntryRhsRules
    $+$ vcat' (map (uncurry (prOneCat knownEmpty False)) groups)
  where
    groups = ruleGroups cf

    virtEntryCat = Cat "BNFCStart"
    virtEntryRhsCats =
      (if hasEntryPoint cf then List1.toList else List1.take 1)
      (allEntryPoints cf)
    virtEntryRhsRules = toVirtRule <$> virtEntryRhsCats

    toVirtRule rhsCat =
      npRule
        (identCat virtEntryCat ++ "_" ++ identCat rhsCat)
        virtEntryCat
        [Left rhsCat]
        Parsable

    knownEmpty = fixPointKnownEmpty ((virtEntryCat, virtEntryRhsRules) : groups)

prTokenRules :: CF -> [(Reg, TokenCat)] -> Doc
prTokenRules cf lexTokens = vcat' (map prOneToken usedTokens)
  where
    usedTokens = filter (isUsedCat cf . TokenCat . snd) lexTokens

-- | Generate one tree-sitter rule for one terminal token.
prOneToken :: (Reg, TokenCat) -> Doc
prOneToken (reg, name) =
  defineSymbol (formatCatName False $ TokenCat name)
    $+$ indent (text $ printRegJSReg reg) <> ","

-- | Generates one tree-sitter rule for one non-terminal from CF.
prOneCat :: KnownEmpty -> Bool -> NonTerminal -> [Rule] -> Doc
prOneCat knownEmpty allowEmpty nt rules =
  defineSymbol (formatCatName False nt)
    $+$ indent (appendComma (wrapRhs parRhs))
  where
    wrapRhs = applyWhen (allowEmpty && Left nt `isKnownEmpty` knownEmpty) $
      wrapOptional'

    (parsableRules, _) = List.partition isParsable rules

    parRhs = wrapChoice (genRules parsableRules)

    genRules = map genRule
    genRule rule =
      ("//" <+> text (renderOneLine (pretty rule)) <+> ";")
      $+$ (formatRhs . transformEmptyMatches knownEmpty) (rhsRule rule)

    renderOneLine = renderStyle (style { mode = OneLineMode })

-- * Builds right-hand side of rules

-- | Format right hand side into list of strings
formatRhs :: [OptSentForm] -> Doc
formatRhs = wrapChoice . map formatSent

formatSent :: OptSentForm -> Doc
formatSent = wrapSeq . map fmtOpt
  where
    fmtOpt (Optional x) = wrapOptional (fmt x)
    fmtOpt (NonOptional x) = fmt x

    fmt (Left c) = text $ refName $ formatCatName False c
    fmt (Right term) = cstring term

-- | Format string for cat name, prefix "_" if the name is for internal rules
formatCatName :: Bool -> Cat -> String
formatCatName internal c =
  if internal
    then "_" ++ formatted
    else formatted
  where
    formatted = formatName c
    formatName (Cat name) = name
    formatName (TokenCat name) = "token_" ++ name
    formatName (ListCat c) = "list_" ++ formatName c
    formatName (CoercCat name i) = name ++ show i


-- * Treesitter-related formatting helpers

-- | Start a defined symbol block in tree-sitter grammar
defineSymbol :: String -> Doc
defineSymbol name = hsep [text name <> ":", text "$", text "=>"]

wrapSeq :: [Doc] -> Doc
wrapSeq = wrapOptListFun "seq" False

wrapChoice :: [Doc] -> Doc
wrapChoice = wrapOptListFun "choice" True

wrapOptional :: Doc -> Doc
wrapOptional = wrapFun "optional" False

wrapOptional' :: Doc -> Doc
wrapOptional' = wrapFun "optional" True

-- | Wrap list using tree-sitter fun if the list contains multiple items
-- Returns the only item without wrapping otherwise
wrapOptListFun :: String -> Bool -> [Doc] -> Doc
wrapOptListFun _   _ [x] = x
wrapOptListFun fun _ [ ] = wrapFun fun False empty
wrapOptListFun fun newline list = wrapFun fun newline (commaJoin newline list)

wrapFun :: String -> Bool -> Doc -> Doc
wrapFun fun newline arg = joinOp [text fun <> text "(", indentOp arg, text ")"]
  where
    joinOp = if newline then vcat' else hcat
    indentOp = if newline then indent else id

-- | Helper for referring to non-terminal names in tree-sitter
refName :: String -> String
refName = ("$." ++)

-- * Generic formatting helpers

-- | Indent one level of 2 spaces
indent :: Doc -> Doc
indent = nest 2

appendComma :: Doc -> Doc
appendComma = (<> text ",")

commaJoin :: Bool -> [Doc] -> Doc
commaJoin newline =
  foldl comma empty
  where
    commaString = if newline then "," else ", "
    comma a b
      | isEmpty a = b
      | isEmpty b = a
      | otherwise = (if newline then ($+$) else (<>)) (a <> commaString) b

