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

import BNFC.Abs (Reg (RSeq, RSeqs, RStar, RAny))
import BNFC.Backend.TreeSitter.RegToJSReg
import BNFC.CF
import BNFC.Lexing (mkRegMultilineComment)
import BNFC.PrettyPrint
import Prelude hiding ((<>))

-- | Indent one level of 2 spaces
indent :: Doc -> Doc
indent = nest 2

-- | Create content of grammar.js file
cfToTreeSitter :: String -> CF -> Doc
cfToTreeSitter name cf =
  -- Overall structure of grammar.js
  text "module.exports = grammar({"
    $+$ indent
      ( text "name: '" <> text name <> text "',"
          $+$ extrasSection
          $+$ wordSection
          $+$ rulesSection
      )
    $+$ text "});"
  where
    extrasSection = prExtras cf
    wordSection = prWord cf
    rulesSection =
      text "rules: {"
        $+$ indent
          ( prRules cf
              $+$ prUsrTokenRules cf
              $+$ prBuiltinTokenRules cf
          )
        $+$ text "},"

-- | Print rules for comments
prExtras :: CF -> Doc
prExtras cf =
  if extraNeeded
    then
      defineSymbol "extras" <> "["
        $+$ indent
          ( -- default rule for white spaces
            text "/\\s/,"
              $+$ mRules
              $+$ sRules
          )
        $+$ text "],"
    else empty
  where
    extraNeeded = length commentMRules + length commentSRules > 0
    (commentMRules, commentSRules) = comments cf
    mRules = vcat' $ map mkOneMRule commentMRules
    sRules = vcat' $ map mkOneSRule commentSRules
    mkOneSRule s = text (printRegJSReg $ RSeq (RSeqs s) (RStar RAny)) <> text ","
    mkOneMRule (s, e) = text (printRegJSReg $ mkRegMultilineComment s e) <> text ","

-- | Print word section, this section is needed for tree-sitter
--   to do keyword extraction before any parsing/lexing, see
--   https://tree-sitter.github.io/tree-sitter/creating-parsers#keyword-extraction
--   TODO: currently, we just add every user defined token as well
--   as the predefined Ident token to this list to be safe. Ideally,
--   we should enumerate all defined tokens against all occurrences of
--   keywords. Any tokens patterns that could accept a keyword will go
--   into this list. This will require integration of a regex engine.
prWord :: CF -> Doc
prWord cf =
  if wordNeeded
    then
      defineSymbol "word"
        $+$ indent
          ( wrapChoice
              ( usrTokensFormatted
                  ++ [text "$.token_Ident" | identUsed]
              )
          )
          <> ","
    else empty
  where
    wordNeeded = identUsed || usrTokens /= []
    identUsed = isUsedCat cf (TokenCat catIdent)
    usrTokens = tokenPragmas cf
    usrTokensFormatted =
      map (text . refName . formatCatName False . TokenCat . fst) $ usrTokens

-- | Print builtin token rules according to their usage
prBuiltinTokenRules :: CF -> Doc
prBuiltinTokenRules cf =
  ifC catInteger integerRule
    $+$ ifC catDouble doubleRule
    $+$ ifC catChar charRule
    $+$ ifC catString stringRule
    $+$ ifC catIdent identRule
  where
    ifC cat d = if isUsedCat cf (TokenCat cat) then d else empty

-- | Predefined builtin token rules
integerRule, doubleRule, charRule, stringRule, identRule :: Doc
integerRule = defineSymbol "token_Integer" <+> text "/\\d+/" <> ","
doubleRule = defineSymbol "token_Double" <+> text "/\\d+\\.\\d+(e-?\\d+)?/" <> ","
charRule =
  defineSymbol "token_Char" <+> text "/'([^'\\\\]|(\\\\[\"'\\\\tnrf]))'/" <> ","
stringRule =
  defineSymbol "token_String" <+> text "/\"([^'\\\\]|(\\\\[\"'\\\\tnrf]))*\"/" <> ","
identRule =
  defineSymbol "token_Ident" <+> text "/[a-zA-Z][a-zA-Z\\d_']*/" <> ","

-- | First print the entrypoint rule, tree-sitter always use the
--   first rule as entrypoint and does not support multi-entrypoint.
--   Then print rest of the rules
prRules :: CF -> Doc
prRules cf =
  if onlyOneEntry
    then
      prOneCat entryRules entryCat
        $+$ prOtherRules entryCat cf
    else error "Tree-sitter only supports one entrypoint"
  where
    --If entrypoint is defined, there must be only one entrypoint
    --If it is not defined, defaults to use the first rule as entrypoint
    onlyOneEntry = not (hasEntryPoint cf) || onlyOneEntryDefined
    onlyOneEntryDefined = length (allEntryPoints cf) == 1
    entryCat = firstEntry cf
    entryRules = rulesForCat' cf entryCat

-- | Print all other rules except the entrypoint
prOtherRules :: Cat -> CF -> Doc
prOtherRules entryCat cf = vcat' $ map mkOne rules
  where
    rules = [(c, r) | (c, r) <- ruleGroupsInternals cf, c /= entryCat]
    mkOne (cat, rules) = prOneCat rules cat

prUsrTokenRules :: CF -> Doc
prUsrTokenRules cf = vcat' $ map prOneToken tokens
  where
    tokens = tokenPragmas cf

-- | Check if a set of rules contains internal rules
hasInternal :: [Rule] -> Bool
hasInternal = not . all isParsable

-- | Generates one or two tree-sitter rule(s) for one non-terminal from CF.
-- Uses choice function from tree-sitter to combine rules for the non-terminal
-- If the non-terminal has internal rules, an internal version of the non-terminal
-- will be created (prefixed with "_" in tree-sitter), and all internal rules will
-- be sectioned as such.
prOneCat :: [Rule] -> NonTerminal -> Doc
prOneCat rules nt =
  defineSymbol (formatCatName False nt)
    $+$ indent (appendComma parRhs)
    $+$ internalRules
  where
    int = hasInternal rules
    internalRules =
      if int
        then defineSymbol (formatCatName True nt) $+$ indent (appendComma intRhs)
        else empty
    parRhs = wrapChoice $ transChoice ++ genChoice (filter isParsable rules)
    transChoice = [text $ refName $ formatCatName True nt | int]
    intRhs = wrapChoice $ genChoice (filter (not . isParsable) rules)
    genChoice = map (wrapSeq . formatRhs . rhsRule)

-- | Generate one tree-sitter rule for one defined token
prOneToken :: (TokenCat, Reg) -> Doc
prOneToken (cat, exp) =
  defineSymbol (formatCatName False $ TokenCat cat)
    $+$ indent (text $ printRegJSReg exp) <> ","

-- | Start a defined symbol block in tree-sitter grammar
defineSymbol :: String -> Doc
defineSymbol name = hsep [text name <> ":", text "$", text "=>"]

appendComma :: Doc -> Doc
appendComma = (<> text ",")

commaJoin :: Bool -> [Doc] -> Doc
commaJoin newline =
  foldl comma empty
  where
    comma a b
      | isEmpty a = b
      | isEmpty b = a
      | otherwise = (if newline then ($+$) else (<>)) (a <> ",") b

wrapSeq :: [Doc] -> Doc
wrapSeq = wrapOptListFun "seq" False

wrapChoice :: [Doc] -> Doc
wrapChoice = wrapOptListFun "choice" True

-- | Wrap list using tree-sitter fun if the list contains multiple items
-- Returns the only item without wrapping otherwise
wrapOptListFun :: String -> Bool -> [Doc] -> Doc
wrapOptListFun fun newline list =
  if length list == 1
    then head list
    else wrapFun fun newline (commaJoin newline list)

wrapFun :: String -> Bool -> Doc -> Doc
wrapFun fun newline arg = joinOp [text fun <> text "(", indent arg, text ")"]
  where
    joinOp = if newline then vcat' else hcat

-- | Helper for referring to non-terminal names in tree-sitter
refName :: String -> String
refName = ("$." ++)

-- | Format right hand side into list of strings
formatRhs :: SentForm -> [Doc]
formatRhs =
  map (\case
    Left c -> text $ refName $ formatCatName False c
    Right term -> quoted term)

quoted :: String -> Doc
quoted s = text "\"" <> text s <> text "\""

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