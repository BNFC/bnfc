
{-  
    BNF Converter: Python abstract syntax and parsing definitions generator
    Copyright (C) 2024  Author: Bjorn Werner
    Based on CFtoCAbs.hs, Copyright (C) 2004 Michael Pellauer
-}

module BNFC.Backend.Python.CFtoPyAbs (cf2PyAbs) where
import Data.List     ( nub, intercalate )
import BNFC.CF
import BNFC.Backend.Python.PyHelpers
import BNFC.Backend.Python.RegToFlex (printRegFlex, escapeChar)
import BNFC.Backend.Common.NamedVariables
import Text.PrettyPrint (Doc, render)
import Data.Either   (lefts)
import Data.Char                    (toLower, toUpper)
import qualified Data.List.NonEmpty as List1


-- | The result is ParsingDefs.py & Absyn.py
cf2PyAbs
  :: String
  -> CF     -- ^ Grammar.
  -> (String, String) -- ParsingDefs.py, Absyn.py.
cf2PyAbs pkgName cf = ( unlines 
  [ "from lark import Lark, Transformer, v_args"
  , "from dataclasses import dataclass"
  , "from " ++ pkgName ++ ".Absyn import *"
  , ""
  , createGrammar cf
  , createTransformer cf
  , ""
  ]
  , unlines 
  ["from typing import List as _List"
  ,"# Value categories (no coercsions):"
  , unlines valueCatsClasses 
  , ""
  , placeholderVariableClass
  , ""
  ,"# Rules:"
  ,"from dataclasses import dataclass, field"
  ,"\n" ++ (unlines dataClasses)
  ]
  )
  where
    rules = cfgRules cf
    
    -- To create Absyn.py
    dataClasses :: [String]
    dataClasses = map makePythonClass
      [ r | r <- rules, not (isDefinedRule r)
      , not (isNilCons r)
      , not (isCoercion r)
      ]

    rulesNoListConstructors = 
      [r | r <- (cfgRules cf), not (isNilCons r), not (isCoercion r) ]
    
    -- Note: Custom tokens are set to inherit "str".
    valueCatNames = nub $ 
      (map (show . normCat . valCat) rulesNoListConstructors) ++ 
      (map (++"(str)") (tokenNames cf)) ++ 
      [ "String(str)"
      , "Char(str)"
      , "Ident(str)"
      , "Integer(int)"
      , "Double(float)"
      ]
    valueCatsClasses = map createValueCatClass valueCatNames


-- Creates a grammar for Lark. Not that it is a real string (r"...").
createGrammar :: CF -> String
createGrammar cf = unlines 
  [ "grammar = r\"\"\""
  , "  ?start_: " ++ entryOrClause
  , ""
  , unlines orClauses
  , larkLiterals cf
  , unlines singleComments
  , unlines multiComments
  , "  %import common.WS"
  , "  %ignore WS"
  , "\"\"\""
  ]
  where
    aCats = reallyAllCats cf
    rs = cfgRules cf

    enumeratedRules :: [(Int, Rul RFun)]
    enumeratedRules = enumerateAllDefinedRules rs 1 []
    orClauses = map (createOrClause cf enumeratedRules) aCats

    (multiMatchers, singleMatchers) = comments cf
    singleComments = map createLineCommentMatcher singleMatchers
    multiComments = map createMultiLineCommentMatcher multiMatchers

    strListEntryPoints = map ((map toLower) .  translateToList . show) 
      ((List1.toList . allEntryPoints) cf)
    entryOrClause = intercalate "\n  | " strListEntryPoints


-- Enumerates all (only defined relevant) rules to prevent naming overlap.
enumerateAllDefinedRules :: [Rul RFun] -> Int -> [(Int, Rul RFun)]
  -> [(Int, Rul RFun)]
enumerateAllDefinedRules [] _ irs = irs
enumerateAllDefinedRules (r:rs) n irs
  | isDefinedRule r = enumerateAllDefinedRules rs (n+1) (irs ++ [(n, r)])
  | otherwise = enumerateAllDefinedRules rs n (irs ++ [(0, r)]) 


-- Creates an or clause with all rules for a given category.
createOrClause :: CF -> [(Int, Rul RFun)] -> Cat -> String
createOrClause cf irs c = unlines
  [ "  ?" ++ map toLower (translateToList (show c)) ++ ": " ++ 
    intercalate "\n  | " 
      (map createProdAndNameForRule catsIrs)
  ]
  where
    catsIrs = [(n, removeWhiteSpaceSeparators r) | (n, r) <- irs, 
      valCat r == c, isParsable r]


-- Creates an entry for an or clause.
createProdAndNameForRule :: (Int, Rul RFun) -> String
createProdAndNameForRule (n, r) =  prodToDocStr (rhsRule r) ++ 
  if (not (isCoercion r)) then " -> " ++ map toLower name else ""
  where
    name
      | isNilFun r = "nil" ++ (identCat . valCat) r
      | isOneFun r = "one" ++ (identCat . valCat) r
      | isConsFun r = "cons" ++ (identCat . valCat) r
      | isDefinedRule r = "d" ++ show n ++ "_r_" ++ funName r
      | otherwise = "r_" ++ funName r 


-- Creates the literals for a grammar for Lark.
larkLiterals :: CF -> String
larkLiterals cf = unlines $ concat
  [ 
  ifC catString  [createLiteral "String.2" "\"(\\\\.|[^\"])*\""]
  , ifC catChar  [createLiteral "Char.2" "\\'(\\\\x[0-9a-f][0-9a-f]|\\\\?[\\S\\s])\\'"]
  , ifC catDouble  [createLiteral "Double.2" "\\d+\\.\\d+(e-?\\d+)?"]
  , ifC catInteger [createLiteral "Integer.2" "\\d+"]
  -- Prolog requires user defined tokens to have priority over Ident; C 
  -- requires Double to have priority over user defined tokens, as C has
  -- "CDouble" matching "3." in 3.14. The lexer definitions rely on the order
  -- for priority, not the length.
  , userDefTokens 
  , ifC catIdent   [createLiteral "Ident" "[A-Za-z]\\w*"]
  ]
  where
    ifC :: TokenCat -> [String] -> [String]
    ifC cat s = if isUsedCat cf (TokenCat cat) then s else []

    userDefTokens :: [String] 
    userDefTokens = [
      createLiteral (name) (printRegFlex exp) | (name, exp) <- tokenPragmas cf
      ]

    createLiteral :: String -> String -> String
    createLiteral name regex = 
      "  " ++ map toUpper name ++ ": /" ++ regex ++ "/"


-- Creates the class transformer, where each member method tells Lark how
-- to transform some parsed node in the tree. 
createTransformer :: CF -> String
createTransformer cf = unlines 
  [ "#transformer"
  , "class TreeTransformer(Transformer):"
  , unlines (map createRuleTransform rs)
  , unlines (map (makeDefineTransform cf) enumeratedRDs)
  , unlines (map createListTransform listRules)
  , createTokenTransformers cf
  ]
  where
    enumeratedRules :: [(Int, Rul RFun)]
    enumeratedRules = enumerateAllDefinedRules (cfgRules cf) 1 []

    rs = [r | r <- cfgRules cf
      , not (isCoercion r)
      , not (isNilCons r)
      , not (isDefinedRule r)]
    listRules = [r | r <- cfgRules cf, isNilCons r]

    enumeratedRDs = [(n, r, d) | (n, r) <- enumeratedRules, d <- definitions cf
      , not (isCoercion r)
      , not (isNilCons r)
      , isDefinedRule r
      , nameCorresponds ((wpThing . defName) d) (funName r)]


-- Creates a transform for a rule
createRuleTransform :: Rul RFun -> String
createRuleTransform r = unlines
  [ "  @v_args(inline=True)"
  , "  def r_" ++ map toLower (funName r) ++ "(self" ++ 
    concat (map (", " ++) enumeratedVars) ++ "):"
  , "    return " ++ funName r ++ "(" ++ intercalate ", " enumeratedVars ++ ")"
  ] 
  where
    sentForm = rhsRule r
    nvCats = numVars sentForm :: [Either (Cat, Doc) String]
    enumeratedVars = [render d | (c, d) <- lefts nvCats]


-- Creates a transform for a list rule.
createListTransform :: Rul RFun -> String
createListTransform r = unlines
  [ "  @v_args(inline=True)"
  , "  def " ++ map toLower name ++ "(self" ++ 
    concat (map (", " ++) enumeratedVars) ++ "):"
  , "    return " ++ args
  ] 
  where
    name
      | isNilFun r = "nil" ++ (identCat . valCat) r
      | isOneFun r = "one" ++ (identCat . valCat) r
      | isConsFun r = "cons" ++ (identCat . valCat) r
      | otherwise = funName r

    sentForm = rhsRule r
    nvCats = numVars sentForm :: [Either (Cat, Doc) String]
    enumeratedVars = [render d | (c, d) <- lefts nvCats]

    args :: String
      | isNilFun r = "[]"
      | isOneFun r = "[" ++ head enumeratedVars ++ "]"
      | isConsFun r = "[" ++ head enumeratedVars ++ "] + " ++ 
        last enumeratedVars
      | otherwise = error "Should be a list function"


-- Creates the transformer functions for the tokens.
createTokenTransformers :: CF -> String
createTokenTransformers cf = unlines $ concat
  [ 
  ifC catString  [createTokenTransform "String"]
  , ifC catChar  [createTokenTransform "Char"]
  , ifC catDouble  [createTokenTransform "Double"]
  , ifC catInteger [createTokenTransform "Integer"]
  -- Prolog requires user defined tokens to have priority over Ident; C 
  -- requires Double to have priority over user defined tokens, as C has
  -- "CDouble" matching "3." in 3.14. The lexer definitions rely on the order
  -- for priority, not the length.
  , userDefTokens 
  , ifC catIdent   [createTokenTransform "Ident"]
  ]
  where
  ifC :: TokenCat -> [String] -> [String]
  ifC cat s = if isUsedCat cf (TokenCat cat) then s else []

  userDefTokens :: [String] 
  userDefTokens = [
    createTokenTransform name | (name, exp) <- tokenPragmas cf
    ]


-- Creates a transform for a token.
createTokenTransform :: String -> String
createTokenTransform name = unlines
  [ "  @v_args(inline=True)"
  , "  def " ++ map toUpper name ++ "(self, token):"
  , "    return " ++ name ++ "(token.value)"
  ] 


-- | Produces the production in the docstring for the parsing definitions.
prodToDocStr ::[Either Cat String] -> String
prodToDocStr [] = ""
prodToDocStr (ec:[]) = ecsToDocStr ec
prodToDocStr (ec:ecs) = 
  ecsToDocStr ec ++ " " ++ prodToDocStr ecs


-- Converts a single element in the production.
ecsToDocStr :: Either Cat String -> String
ecsToDocStr (Left (TokenCat t))       = map toUpper t 
ecsToDocStr (Left c)       = map toLower (translateToList (show c))
ecsToDocStr (Right strOp)  = "\"" ++ concat (map escapeBackslash strOp) ++ "\""


-- | For single-line comments
createLineCommentMatcher :: String -> String
createLineCommentMatcher r = unlines
  [ "  C" ++ toOrd r ++ ": /" ++ concat (map escapeChar r) ++ "[^\\n]*/"
  , "  %ignore C" ++ toOrd r
  ]


-- | For multi-line comments
createMultiLineCommentMatcher :: (String, String) -> String
createMultiLineCommentMatcher (s, e) = unlines
  [ "  C" ++ toOrd (s ++ e) ++ ": /" ++ escaped s ++ "([\\s\\S]*?)" ++ 
    escaped e ++ "/"
  , "  %ignore C" ++ toOrd (s ++ e)
  ]
  where
    escaped s = concat $ map escapeChar s


-- Since we're using a real string for the grammar,  r""" ... """ it seems 
-- we can't escape everything in strOp from regflex. Only backslashes.
escapeBackslash :: Char -> String
escapeBackslash '\\' = "\\\\"
escapeBackslash c = [c]


-- | To compare names for defines. The first letter needs to be lowered, so
--   "while" == "While".
nameCorresponds :: String -> String -> Bool
nameCorresponds (x:xs) (y:ys) = (toLower x == toLower y) && (xs == ys)
nameCorresponds _ _ = error "Names can't be empty"


-- Creates a transformer for a rule with its corresponding define.
makeDefineTransform :: 
  CF -> (Int, Rul RFun, Define) -> String
makeDefineTransform cf (n, defRule, defi)  = unlines
  [ "  @v_args(inline=True)"
  , "  def d" ++ show n ++ "_r_" ++ map toLower name ++ "(self" ++ 
    concat (map (", " ++) enumeratedVars) ++ "):"
  , "    return " ++ expToDef env2 (defBody defi)
  , ""
  ]
  where
    name = (wpThing . defName) defi
    sentForm = rhsRule defRule
    args = map fst (defArgs defi)
    nvCats = numVars sentForm :: [Either (Cat, Doc) String]
    enumeratedVars = [render d | (c, d) <- lefts nvCats]
    env2 = zip args enumeratedVars


-- | Converts the production of a define, called an expression, to a
--   production for the parsing definition.
expToDef :: [(String, String)] -> Exp -> String
expToDef env (App "(:)" _ (e:[App "[]" _ _])) = expToDef env e ++ "]"
expToDef env (App "(:)" _ (e:[recList])) = "[" ++ expToDef env e ++ ", " ++
  expToDef env recList
expToDef _ (App "[]" _ _) = "[]"
expToDef env (App fName _ exps) = 
  fName ++ "(" ++ addCommas (map (expToDef env) exps) ++ ")"
expToDef env (Var s) = case lookup s env of
  Just p -> p
  Nothing -> error "Missing variable in define enviroment"
expToDef _ (LitInt i) = "Integer(" ++ show i ++ ")"
expToDef _ (LitDouble d) = "Double(" ++ show d ++ ")"
expToDef _ (LitChar s) = "Char(\"" ++ show s ++ "\")"
expToDef _ (LitString s) = "String('" ++ show s ++ "')"


-- A placeholder variable to store additional information, for say type
-- annotation.
placeholderVariableClass :: String
placeholderVariableClass = unlines 
  [ "# Placeholder to add additional information to a node in the AST," ++
      " like type information."
  , "class _AnnType:"
  , "  def __init__(self):"
  , "    self.__v = None"
  , ""
  , "  def s(self, val):"
  , "    if not self.__v == None:"
  , "      if self.__v != val:"
  , "        raise Exception('already has type: ' + str(self.__v)" ++
      " + ' and tried to set to ' + str(val))"
  , "    self.__v = val"
  , ""
  , "  def g(self):"
  , "    return self.__v"
  , ""
  , "  def __str__(self):"
  , "    return str(self.__v.__class__)"
  , ""
  , "  def __repr__(self):"
  , "    return str(self.__v.__class__)"
  ]     


-- | Creates a parsing definition that points to all entrypoints.
createCommonEntrypointDef :: CF -> String
createCommonEntrypointDef cf = unlines 
  [ "def p__Start(p):"
  , "  '''"
  , "  _Start : " ++ (translateToList . show . head) cats ++
    concat (map createCase (tail cats))
  , "  '''"
  , "  p[0] = p[1]"
  , ""
  , ""
  , "# Comment the below line to enable the '_Start' entrypoint (may yield"
    ++ " conflict warnings)."
  , "del p__Start"
  , ""
  ]
  where
    cats = (List1.toList . allEntryPoints) cf

    createCase :: Cat -> String
    createCase c = "\n         | " ++ translateToList (show c)


-- | The value categories become abstract classes, for type hinting.
createValueCatClass :: String -> String
createValueCatClass s = "class " ++ s ++ ":\n\tpass\n"


-- | Make a Python class from a rule's name and production.
makePythonClass :: Rul RFun -> String
makePythonClass rule = 
  "@dataclass\n" ++
  "class " ++ name ++ ":\n" ++
    if length cats == 0 then "\tpass\n" else classBody
  where
    name = funName rule
    sentForm = rhsRule rule
    cats = lefts sentForm
    nvCats = numVars sentForm :: [Either (Cat, Doc) String]

    enumeratedVarsWithType = [render d ++ ": " ++ 
      strCatToPyTyping (show (normCat c)) | (c, d) <- lefts nvCats]

    classBody = unlines $ map ("\t" ++) (enumeratedVarsWithType ++ 
      ["_ann_type: _AnnType = field(default_factory=_AnnType)"])


-- | Creates the corresponding type hinting for some member variable.
strCatToPyTyping :: String -> String
strCatToPyTyping s = 
  if strIsList s then "_List['" ++ (tail . init) s ++ "']" else s


