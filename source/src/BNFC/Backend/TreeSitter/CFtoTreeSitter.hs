{-
    BNF Converter: TreeSitter Grammar Generator

    Description   : This module converts BNFC grammar to the contents of a
                    tree-sitter grammar.js file

    Author        : Kangjing Huang (huangkangjing@gmail.com)
    Created       : 08 Nov, 2023

-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module BNFC.Backend.TreeSitter.CFtoTreeSitter where

import BNFC.Abs (Reg (RSeq, RSeqs, RStar, RAny))
import BNFC.Backend.TreeSitter.RegToJSReg
    ( escapeCharFrom, printRegJSReg )
import BNFC.CF
import BNFC.Lexing (mkRegMultilineComment)
import BNFC.PrettyPrint

import Prelude hiding ((<>))
import qualified Data.Map as Map
import Data.Map ()

-- | Indent one level of 2 spaces
indent :: Doc -> Doc
indent = nest 2

-- | Create content of grammar.js file
cfToTreeSitter :: String -- ^ Name of the language
                -> CF    -- ^ Context-Free grammar of the language
                -> Bool  -- ^ Flags to enable zero-width match elimination
                -> Doc   -- ^ grammar.js file generated
cfToTreeSitter name cf removeZero =
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
          ( prRules cf removeZero
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
      map (text . refName . formatCatName False . TokenCat . fst) usrTokens

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

-- Tree Sitter does not support empty strings well enough
-- (Ref: https://github.com/tree-sitter/tree-sitter/issues/98), thus we need to
-- handle empty strings differently using the optional keyword
-- Crucially, any named symbols in Tree Sitter cannot have a zero width match,
-- unless it is the start symbol
-- Thus, we look for all categories that match zero width, tag them as "optional"
-- And when that cat is referred to, we use `optional($.cat_name)` in place of
-- `$.cat_name` to circumvent this issue

-- | Cat with a tag indicating if it is optional
data CatOpt' a = Always a | Optional a
type CatOpt = CatOpt' Cat

-- | type class for OCat or Cat
class UnwrapCat a where
  unwrap:: a -> Cat

instance UnwrapCat Cat where
  unwrap = id

instance UnwrapCat CatOpt where
    -- | unwrap to original Cat
  unwrap c = case c of
    Always ct -> ct
    Optional ct -> ct

-- | Rule with RHS tagged
data RuleOpt = Rule' {srcRule::Rule, taggedRhs::SentFormOpt}
type SentFormOpt = [Either CatOpt String]

-- | type class for a rule data type that can be formatted
class FormatRule a where
  -- | get the original rule
  getRule:: a -> Rule
  -- | format the RHS of the rule
  formatRuleRhs:: a -> [Doc]

instance FormatRule Rule where
  getRule = id

  formatRuleRhs r = 
    map (\case
      Left c -> text $ refName $ formatCatName False c
      Right term -> quoted term) $ rhsRule r


instance FormatRule RuleOpt where
  getRule = srcRule

  formatRuleRhs r = 
    map (\case
      Left (Always c) -> text $ refName $ formatCatName False c
      Left (Optional c) -> wrapFun "optional" False $
        text (refName $ formatCatName False c)
      Right term -> quoted term) $ taggedRhs r

-- | Analyzes the grammar with the entrance symbol.
-- This function finds all rules for the entrance symbol, and groups
-- all remaining categories with their rules, including internal rules.
analyzeCF :: CF -- ^ Context-free grammar of the language
  -> Cat -- ^ Category object for the entrance symbol
  -> ([(Cat, [Rule])], [Rule]) -- ^ (groups of remaining categories and rules, entrance rules)
analyzeCF cf entryCat = ([(c, rulesForCat' cf c)| c <- allCats, c /= entryCat],
  rulesForCat' cf entryCat)
  where allCats = reallyAllCats cf

-- | Analyzes the grammar with the entrance symbol.
-- This version of analyze function performs zero-width match analysis on all symbols and
-- returns with optional flags determined and tagged for all returning Cats and rules.
-- Returns (list of remaining tagged categories and tagged rules, tagged entrance rules)
analyzeCFOptional :: CF  -- ^ Context-free grammar of the language
  -> Cat -- ^ Category object of the entrance symbol
  -> ([(CatOpt, [RuleOpt])], [RuleOpt]) -- ^ (groups of tagged remaining categories and tagged rules, tagged entrance rules)
analyzeCFOptional cf entryCat =
  (
    -- Empty rules are excluded from normal categories since they are handled by
    -- "optional()" keywords in tree-sitter
    [(wrapCat c, 
      map wrapRule $ filter (not . ruleIsEmpty) $ rulesForCat' cf c)
      | c <- allCats, c /= entryCat],
    -- Tree-sitter should support zero-width matches with root (i.e. entrance) symbol
    -- thus no need to filter them out
    map wrapRule $ rulesForCat' cf entryCat
  )
  where
    allCats = reallyAllCats cf
    -- Stores mapping from Cat to optional flag
    -- Currently we only recognize optional rules if any RHS of the rules is empty
    -- list, and ignore more complex cases.
    -- Complex optional cases may trigger tree-sitter to fail or bug out.
    catOptMap = Map.fromList $
      map (\c -> (c, any ruleIsEmpty (rulesForCat' cf c)))
      -- Always format entrance symbols as non-optional since
      -- tree-sitter should support zero-width matches on them 
      $ filter (/= entryCat) allCats
    -- Tags Cat to Cat' using catOptMap
    wrapCat:: Cat -> CatOpt
    wrapCat c = if Map.findWithDefault False c catOptMap
      then Optional c
      else Always c
    wrapRule r = Rule' {srcRule = r, 
      taggedRhs = map wrapSentFormItem $ rhsRule r
    }
    wrapSentFormItem :: Either Cat String -> Either CatOpt String
    wrapSentFormItem (Left c) = Left $ wrapCat c
    wrapSentFormItem (Right s) = Right s
    ruleIsEmpty = null . rhsRule . getRule

-- | First print the entrypoint rule, tree-sitter always use the
--   first rule as entrypoint and does not support multi-entrypoint.
--   Then print rest of the rules
prRules :: CF -> Bool -> Doc
prRules cf removeZero =
  if onlyOneEntry
    then
      -- entry rules are formatted without optional
      -- tree-sitter should support zero-width (a.k.a empty) matches for top level symbols
      if removeZero
        then
          prOneCat entryRulesOpt entryCat
            $+$ prOtherRules otherCatRulesOpt
        else
          prOneCat entryRules entryCat
            $+$ prOtherRules otherCatRules
    else error "Tree-sitter only supports one entrypoint"
  where
    --If entrypoint is defined, there must be only one entrypoint
    --If it is not defined, defaults to use the first rule as entrypoint
    onlyOneEntry = not (hasEntryPoint cf) || onlyOneEntryDefined
    onlyOneEntryDefined = length (allEntryPoints cf) == 1
    entryCat = firstEntry cf
    (otherCatRulesOpt, entryRulesOpt) = analyzeCFOptional cf entryCat
    (otherCatRules, entryRules) = analyzeCF cf entryCat

-- | Print all other rules except the entrypoint
prOtherRules :: (UnwrapCat a, FormatRule b) => [(a, [b])] -> Doc
prOtherRules otherRules = vcat' $ map mkOne otherRules
  where
    mkOne (cat, rules) = prOneCat rules $ unwrap cat

prUsrTokenRules :: CF -> Doc
prUsrTokenRules cf = vcat' $ map prOneToken tokens
  where
    tokens = tokenPragmas cf

-- | Check if a set of rules contains internal rules
hasInternal :: (FormatRule a) => [a] -> Bool
hasInternal = not . all (isParsable . getRule)

-- | Generates one or two tree-sitter rule(s) for one non-terminal from CF.
-- Uses choice function from tree-sitter to combine rules for the non-terminal
-- If the non-terminal has internal rules, an internal version of the non-terminal
-- will be created (prefixed with "_" in tree-sitter), and all internal rules will
-- be sectioned as such.
prOneCat :: FormatRule a => [a] -> NonTerminal -> Doc
prOneCat rules nt =
  defineSymbol (formatCatName False $ nt)
    $+$ indent (appendComma parRhs)
    $+$ internalRules
  where
    int = hasInternal rules
    internalRules =
      if int
        then defineSymbol (formatCatName True nt) $+$ indent (appendComma intRhs)
        else empty
    parRhs = wrapChoice $ transChoice ++ genChoice (filter (isParsable . getRule) rules)
    transChoice = [text $ refName $ formatCatName True nt | int]
    intRhs = wrapChoice $ genChoice (filter (not . isParsable. getRule) rules)
    genChoice = map (wrapSeq . formatRuleRhs)

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

stringLiteralReserved:: String
stringLiteralReserved = "\"\\"

escapeStringLiteral:: String -> String
escapeStringLiteral = concatMap $ escapeCharFrom stringLiteralReserved

quoted :: String -> Doc
quoted s = text "\"" <> text (escapeStringLiteral s) <> text "\""

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