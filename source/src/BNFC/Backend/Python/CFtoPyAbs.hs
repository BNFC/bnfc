
{-  
    BNF Converter: Python abstract syntax and parsing definitions generator
    Copyright (C) 2024  Author: Bjorn Werner
    Based on CFtoCAbs.hs, Copyright (C) 2004 Michael Pellauer
-}

module BNFC.Backend.Python.CFtoPyAbs (cf2PyAbs) where
import Data.List     ( nub, intercalate )
import BNFC.CF
import BNFC.Backend.Python.PyHelpers
import BNFC.Backend.Common.NamedVariables
import Text.PrettyPrint (Doc, render)
import Data.Either   (lefts)
import Data.Char                    (toLower)
import qualified Data.List.NonEmpty as List1

-- | The result is ParsingDefs.py & Absyn.py
cf2PyAbs
  :: String
  -> CF     -- ^ Grammar.
  -> [(String, String)] -- Tokens to unicode mapping
  -> (String, String) -- ParsingDefs.py, Absyn.py.
cf2PyAbs pkgName cf tokensPly = ( unlines 
  [ "from " ++ pkgName ++ ".Absyn import *"
  , "\n\n" ++ createCommonEntrypointDef cf
  , "\n\n" ++ (unlines parsingDefs)
  , if length definesParsingDefs > 0 
    then "\n\n# Parsing rules from defines"
    else ""
  , "\n\n" ++ unlines definesParsingDefs
  ]
  , "from typing import List as _List" ++ 
    "\n\n# Value categories (no coercsions):" ++
    "\n\n" ++ unlines valueCatsClasses ++ 
    "\n\n" ++ placeholderVariableClass ++ 
    "\n\n# Rules:" ++
    "\n" ++ "from dataclasses import dataclass, field" ++
    "\n\n" ++ (unlines dataClasses)
  )
  where
    rules = cfgRules cf

    -- To create ParsingDefs.py 
    parsingDefs :: [String]
    parsingDefs = map (ruleToParsingDef cf tokensPly)
      [r | r <- rules, isParsable r, not (isDefinedRule r)]
    
    definesParsingDefs = makeDefineParsingDefs cf tokensPly

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


-- | Creates a parsing definition, by checking what type of rule it is and
--   calling the corresponding make function.
ruleToParsingDef :: CF -> [(String, String)] -> Rul RFun -> String
ruleToParsingDef cf tokensPly rule 
  | isCoercion funcRStr = 
    makeParseCoercion cf tokensPly funcCat (fName, sentForm)
  | isNilFun funcRStr = 
    makeParseNil tokensPly funcCat (fNameTranslated, sentForm)
  | isOneFun funcRStr = 
    makeParseOne cf tokensPly funcCat (fNameTranslated, sentForm)
  | isConsFun funcRStr = 
    makeParseCons cf tokensPly funcCat (fNameTranslated, sentForm)
  | isDefinedRule rule = 
    error "Should not generate define rules in this step"
  | otherwise = 
    makeParseFunc cf tokensPly funcCat (fName, sentForm)
  where
    funcRStr = funRule rule :: RString
    fName = wpThing funcRStr :: String

    funcCat = valCat rule :: Cat
    catStr = show (valCat rule) :: String

    fNameTranslated :: String
    fNameTranslated
      | isNilFun funcRStr = catStr
      | otherwise = fName

    sentForm = rhsRule rule :: [Either Cat String]


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


-- | It could be this is only guarding against list categories.
literalsToPytypeMaybe :: CF -> String -> Maybe String
literalsToPytypeMaybe cf s = case s of
  "Integer" -> Just "Integer"
  "Double" -> Just "Double"
  "Char"   -> Just "Char"
  "String" -> Just "String"
  "Ident" -> Just "Ident"
  _ -> if s `elem` (tokenNames cf) then Just s else Nothing


-- | The following makeParse functions create their corresponding parsing 
--   definitions for some rule.
makeParseFunc :: CF -> [(String, String)] -> Cat -> (String, SentForm)
  -> String
makeParseFunc cf tokensPly dataCat (name, sentForm) = unlines 
  [ "def " ++ "p_" ++ name ++ "(p):\n" ++ "\t" ++ "\"\"\""
  , "\t" ++ (show dataCat) ++ " : " ++ (prodToDocStr tokensPly sentForm)
  , "\t" ++ "\"\"\""
  , "\t" ++ "p[0] = " ++ rhs ++ "\n"
  ]
  where
    rhs =  name ++ "(" ++ (addCommas (getLeftIndexes cf 1 sentForm)) ++  ")"


makeParseCoercion :: CF -> [(String, String)] -> Cat -> (String, SentForm)
  -> String
makeParseCoercion cf tokensPly dataCat (_, sentForm) = unlines
  [ "def " ++ "p_" ++ (show sourceCat) ++ "(p):\n" ++ "\t" ++ "\"\"\""
  , "\t" ++ (show dataCat) ++ " : " ++ (prodToDocStr tokensPly sentForm)
  , "\t" ++ "\"\"\""
  , "\t" ++ "p[0] = " ++ strP ++ "\n"
  ]
  where
    strP = head (getLeftIndexes cf 1 sentForm)
    sourceCat = (head . lefts) sentForm


makeParseNil :: [(String, String)] -> Cat -> (String, SentForm) -> String
makeParseNil tokensPly dataCat (_, sentForm) = unlines
  [ "def " ++ "p_" ++ "Nil" ++ translatedCat ++ "(p):\n" ++ "\t" ++ "\"\"\""
  , "\t" ++ translatedCat ++ " : " ++ (prodToDocStr tokensPly sentForm)
  , "\t" ++ "\"\"\""
  , "\t" ++ "p[0] = []\n"
  ]
  where
    translatedCat = translateToList $ show dataCat


makeParseOne :: CF -> [(String, String)] -> Cat -> (String, SentForm) -> String
makeParseOne cf tokensPly dataCat (_, sentForm) = unlines
  [ "def " ++ "p_" ++ "One" ++ translatedCat ++ "(p):\n" ++ "\t" ++ "\"\"\""
  , "\t" ++ translatedCat ++ " : " ++ (prodToDocStr tokensPly sentForm)
  , "\t" ++ "\"\"\""
  , "\t" ++ "p[0] = " ++ rhs ++ "\n"
  ]
  where
    translatedCat = translateToList $ show dataCat
    rhs = intercalate " + " (getLeftIndexesLists tokensPly cf 1 sentForm)


makeParseCons :: CF -> [(String, String)] -> Cat -> (String, SentForm)
  -> String
makeParseCons cf tokensPly dataCat (_, sentForm) = unlines
  [ "def " ++ "p_" ++ "Cons" ++ translatedCat ++ "(p):\n" ++ "\t" ++ "\"\"\""
  , "\t" ++ translatedCat ++ " : " ++ (prodToDocStr tokensPly sentForm)
  , "\t" ++ "\"\"\"" ++ "\n"
  , "\t" ++ "p[0] = " ++ rhs ++ "\n"
  ]
  where
    translatedCat = translateToList $ show dataCat
    rhs = intercalate " + " (getLeftIndexesLists tokensPly cf 1 sentForm)


-- | Produces a list of the elements in the code production, where the indices
--   match the argument categories.
getLeftIndexesLists :: [(String, String)] -> CF -> Int -> [Either Cat String]
  -> [String]
getLeftIndexesLists _ _ _ [] = []
getLeftIndexesLists tokensPly cf n (Left c:ecs)
  | isList c = [typedPTerm] ++ (getLeftIndexesLists tokensPly cf (n+1) ecs)
  | otherwise = ["[" ++ typedPTerm ++ "]"] ++ 
    (getLeftIndexesLists tokensPly cf (n+1) ecs)
  where
    pTerm = "p[" ++ (show n) ++ "]"
    typedPTerm = case literalsToPytypeMaybe cf (show c) of
      Just s -> s ++ "(" ++ pTerm ++ ")"
      Nothing -> pTerm
getLeftIndexesLists tokensPly cf n (Right strOp:ecs)
  | separatorIsEmpty tokensPly strOp = getLeftIndexesLists tokensPly cf n ecs
  | otherwise = getLeftIndexesLists tokensPly cf (n+1) ecs


-- | In case the deliminator is "" or is not defined for the lexer, like
--   ignored characters.
separatorIsEmpty :: [(String, String)] -> String -> Bool
separatorIsEmpty tokensPly strOp
  | length strOp > 0 = case lookup strOp tokensPly of
    Just _ -> False
    Nothing -> True
  | otherwise = True


-- | Produces a list of the elements in the code production, where the indices
--   match the argument categories.
getLeftIndexes :: CF -> Int -> [Either Cat String] -> [String]
getLeftIndexes _ _ [] = []
getLeftIndexes cf n (Left c:ecs) = [typedPTerm] ++ 
  (getLeftIndexes cf (n+1) ecs)
  where
    pTerm = "p[" ++ (show n) ++ "]"
    typedPTerm = case literalsToPytypeMaybe cf (show c) of
      Just s -> s ++ "(" ++ pTerm ++ ")"
      Nothing -> pTerm
getLeftIndexes cf n (Right _:ecs) = getLeftIndexes cf (n+1) ecs


-- | Produces the production in the docstring for the parsing definitions.
prodToDocStr :: [(String, String)] -> [Either Cat String] -> String
prodToDocStr _ [] = ""
prodToDocStr tokensPly (ec:[]) = ecsToDocStr tokensPly ec
prodToDocStr tokensPly (ec:ecs) = 
  ecsToDocStr tokensPly ec ++ " " ++ prodToDocStr tokensPly ecs


-- Converts a single element in the production.
ecsToDocStr :: [(String, String)] -> Either Cat String -> String
ecsToDocStr _ (Left c)       = translateToList $ show c
ecsToDocStr tokensPly (Right strOp)  = case lookup strOp tokensPly of
  (Just s) -> s
  Nothing -> ("") -- We assume it is no token, this affects getLeftIndexes


-- | Creating the parsing definitions for the defines.
makeDefineParsingDefs :: CF ->  [(String, String)] -> [String]
makeDefineParsingDefs cf tokensPly = defFuncsPy
  where
    rules = cfgRules cf
    
    definedRules :: [Rul RFun]
    definedRules = [r | r <- rules, isDefinedRule r]
  
    pairs :: [(Rul RFun, Define)]
    pairs = [(dr, d) | dr <- definedRules, d <- definitions cf, 
      nameCorresponds ((wpThing . defName) d) (funName dr)]
    
    -- Adds a number to the name to make each define separate.
    numberedPairs = zip [1..] pairs
    defFuncsPy = map (makeDefineParsingDef cf tokensPly) numberedPairs 


-- | To compare names for defines. The first letter needs to be lowered, so
--   "while" == "While".
nameCorresponds :: String -> String -> Bool
nameCorresponds (x:xs) (y:ys) = (toLower x == toLower y) && (xs == ys)
nameCorresponds _ _ = error "Names can't be empty"


-- | Creates a define parsing definition.
makeDefineParsingDef :: 
  CF -> [(String, String)] -> (Int, (Rul RFun, Define)) -> String
makeDefineParsingDef cf tokensPly (n, (defRule, defi))  = unlines
  [ "def p_D" ++ (show n) ++ name ++ "(p):"
  , "\t\"\"\""
  , "\t" ++ translatedCat ++ " : " ++ (prodToDocStr tokensPly sentForm)
  , "\t\"\"\""
  , "\t# " ++ show env
  , "\tp[0] = " ++ expToDef env (defBody defi)
  , ""
  ]
  where
    name = (wpThing . defName) defi
    translatedCat = translateToList $ (catToStr . valCat) defRule
    sentForm = rhsRule defRule
    indexes = getLeftIndexes cf 1 sentForm
    args = map fst (defArgs defi)
    env = zip args indexes


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


