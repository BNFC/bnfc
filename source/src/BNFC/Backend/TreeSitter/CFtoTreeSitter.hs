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

import BNFC.Abs (Reg(..))
import BNFC.Backend.TreeSitter.RegToJSReg
import BNFC.Backend.TreeSitter.MatchesEmpty(fixPointKnownEmpty, transformEmptyMatches, KnownEmpty, OptSym(..), OptSentForm, isKnownEmpty)
import BNFC.CF
import BNFC.Utils(when, applyWhen, mkNames, NameStyle(..))
import BNFC.Lexing (mkLexer, LexType(..), mkRegMultilineComment)
import BNFC.PrettyPrint

import Prelude hiding ((<>))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.List.NonEmpty as List1
import qualified Text.Printf as Printf

-- * Main entry point

-- | Create content of grammar.js file
cfToTreeSitter :: String -> Cat -> CF -> Doc
cfToTreeSitter name wordCat cf =
  -- Overall structure of grammar.js
  text "module.exports = grammar({"
    $+$ indent
      ( text "name:" <+> jsString name <> ","
          $+$ extrasSection
          $+$ wordSection
          $+$ rulesSection
      )
    $+$ text "});"
  where
    lexTokens = Maybe.mapMaybe keepToken (mkLexer cf)
    keepToken (r, LexToken nm) | not (isUnusedBuiltin nm) = Just (r, nm)
    keepToken _ = Nothing
    isUnusedBuiltin x = x `elem` specialCatsP && not (isUsedCat cf (TokenCat x))

    -- generate rules for comment tokens so they can be used in highlighting
    (mlComments, slComments) = comments cf
    commentTokens =
      disambig "CommentSingle" (map treeSitterSingleLineComment slComments)
      ++ disambig "CommentMulti" (map (uncurry mkRegMultilineComment) mlComments)
    disambig base regs = zip regs names
      where names = mkNames (map snd lexTokens) CamelCase (base <$ regs)

    extrasSection = prExtras (map snd commentTokens)
    wordSection = prWord wordCat cf
    rulesSection =
      text "rules: {"
        $+$ indent
          ( prRules cf
              $+$ prTokenRules (lexTokens ++ commentTokens) )
        $+$ text "},"

-- * Functions to build parts of grammar.js

-- | Print rules for comments
prExtras :: [TokenCat] -> Doc
prExtras commentTokens =
  defineSymbol "extras" <> "["
    $+$ indent
      ( -- default rule for white spaces
        text "/\\s/,"
          $+$ vcat' (map (appendComma . text . refName . formatTokenName) commentTokens)
      )
    $+$ text "],"

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

prTokenRules :: [(Reg, TokenCat)] -> Doc
prTokenRules = vcat' . map prOneToken

-- | Generate one tree-sitter rule for one terminal token.
prOneToken :: (Reg, TokenCat) -> Doc
prOneToken (reg, name) =
  defineSymbol (formatTokenName name)
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
    fmt (Right term) = jsString term

formatTokenName :: TokenCat -> String
formatTokenName = formatCatName False . TokenCat

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

-- | Javascript string, including double quotes.
--
-- This uses the following escapes: @\n@ @\r@ @\f@ @\t@, @\uXXXX@ for Unicode
-- code-points 0xFFFF or less, and @\uXXXXXXXX@ for larger unicode code points.
-- ASCII-printable characters are emitted verbatim aside from @"@ and @\@ which
-- are escaped.
jsString :: String -> Doc
jsString = doubleQuotes . text . concatMap jsChar

-- | Javascript character fragment (to be contained within a string).
jsChar :: Char -> String
jsChar '"' = "\\\""
jsChar '\\' = "\\\\"
jsChar '\n' = "\n"
jsChar '\r' = "\r"
jsChar '\t' = "\t"
jsChar '\f' = "\f"
jsChar c = case Char.ord c of
  code | 0x20 <= code && code <= 0x7e -> [c]
  code | code <= 0xffff -> Printf.printf "\\u%04x" code
  code -> Printf.printf "\\U%08x" code

-- | Tree-sitter needs the single-line comment to go up to but /not including/
-- the line terminator, in order to handle files without a final line terminator.
treeSitterSingleLineComment :: String -> Reg
treeSitterSingleLineComment s = RSeqs s `RSeq` RStar (RAny `RMinus` RAlts "\n\r")

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

