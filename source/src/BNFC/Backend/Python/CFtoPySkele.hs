
{-  
    BNF Converter: Python skeleton-code generator
    Copyright (C) 2024  Author: Bjorn Werner
-}

module BNFC.Backend.Python.CFtoPySkele where
import BNFC.CF 
import BNFC.Backend.Python.PyHelpers
import Data.Char     (toLower)
import BNFC.Backend.Common.NamedVariables
import Text.PrettyPrint (Doc, render)
import Data.Either   (lefts)
import Data.List     (intercalate)

-- | Entrypoint.
cf2PySkele :: String -> CF -> String
cf2PySkele pkgName cf = unlines
  ["from " ++ pkgName ++ ".Absyn import *"
  , ""
  , ""
  , makeSkele cf
  ]


-- Creates first a matcher with all value categories, and underneath one
-- matcher for each value category.
makeSkele :: CF -> String
makeSkele cf = unlines 
  [ "# Categories combined into one matcher"
  , "def skeleMatcher(ast: object):"
  , ind 1 "match ast:"
  , intercalate "\n" skeleLiteralCases
  , intercalate "\n" skeleTokenCases
  , intercalate "\n" skeleRuleCases
  , ind 2   "case _:"
  , ind 3     "raise Exception(str(ast.__class__) + ' unmatched')"
  , ""
  , ""
  , "# Categories split into their own matchers"
  , unlines matchersOnCats
  ]
  where
    rules = 
      [ r | r <- cfgRules cf
      , not (isCoercion r)
      , not (isDefinedRule r)
      , not (isNilCons r)
      ]

    presentLiterals = ifC catInteger ++ 
      ifC catDouble ++
      ifC catString ++
      ifC catIdent ++
      ifC catChar
        
    skeleLiteralCases = map makeSkeleTokenCase presentLiterals
    skeleTokenCases = map makeSkeleTokenCase (tokenNames cf)
    skeleRuleCases = map makeSkeleRuleCase rules

    parserCats = filter (not . isList) (allParserCatsNorm cf) :: [Cat]
    rulesfornormalizedcat = map (rulesForNormalizedCat cf) parserCats
    parserCatsWithRules = zip parserCats rulesfornormalizedcat

    matchersOnCats = map makeMatcherOnCat parserCatsWithRules

    ifC :: TokenCat -> [String]
    ifC cat = if isUsedCat cf (TokenCat cat) then [cat] else []


-- Creates a matcher for some value category.
makeMatcherOnCat :: (Cat, [Rul RFun]) -> String
makeMatcherOnCat (c, rules) = unlines
  [ "def matcher" ++ show c ++ "(" ++ varName ++ ": " ++ show c ++ "):"
  , ind 1 "match " ++ varName ++ ":"
  , intercalate "\n" cases
  , ind 2   "case _:"
  , ind 3     "raise Exception(str(" ++ varName ++ ".__class__) + ' unmatched')"
  , ""
  ]
  where
    varName = map toLower (show c) ++ "_"
    cases = map makeSkeleRuleCase (filter
      (\r -> not (isCoercion r) && not (isDefinedRule r))
      rules)


-- | Creates a case for some rule.
makeSkeleRuleCase :: Rul RFun -> String
makeSkeleRuleCase rule = intercalate "\n"
  [ ind 2 "case " ++ name ++ "(" ++ varNamesCommad ++ "):"
  , ind 3   "# " ++ (showEcss sentForm)
  , ind 3   "raise Exception('" ++ name ++ " not implemented')"
  ]
  where
    name = unkw (funName rule)
    sentForm = rhsRule rule
    nvCats = numVars sentForm :: [Either (Cat, Doc) String]
    enumeratedVarNames = [render d | (_, d) <- lefts nvCats]
    varNamesCommad = addCommas (enumeratedVarNames ++ ["_ann_type"])


-- | Creates a case for a user-defined token.
makeSkeleTokenCase :: String -> String
makeSkeleTokenCase tokenName = intercalate "\n"
  [ ind 2 "case " ++ unkw tokenName ++ "():"
  , ind 3   "raise Exception('" ++ unkw tokenName ++ " not implemented')"
  ]

