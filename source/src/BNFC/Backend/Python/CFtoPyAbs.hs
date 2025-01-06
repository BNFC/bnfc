
{-  
    BNF Converter: Python abstract syntax and parsing definitions generator
    Copyright (C) 2024  Author: Bjorn Werner
    Based on CFtoCAbs.hs, Copyright (C) 2004 Michael Pellauer
-}

module BNFC.Backend.Python.CFtoPyAbs (cf2PyAbs) where
import Data.List     (nub)
import Data.Char     (isLower)
import Data.Either   (lefts)
import BNFC.CF
import BNFC.Backend.Python.PyHelpers
import BNFC.Backend.Common.NamedVariables
import Text.PrettyPrint (Doc, render)


-- | Produces the content for Absyn.py
cf2PyAbs :: CF -> String
cf2PyAbs cf = unlines 
  ["from typing import List as _List"
  ,"# Value categories (no coercsions):"
  , unlines valueCatsClasses 
  , ""
  , placeholderVariableClass
  , ""
  ,"# Rules:"
  ,"from dataclasses import dataclass, field"
  ,"\n" ++ (unlines dataClasses)
  , ""
  , createDefineFunctions cf
  ]
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
      (map (unkw . show . normCat . valCat) rulesNoListConstructors) ++ 
      (map ((++ "(str)") . unkw) (tokenNames cf)) ++ 
      [ "String(str)"
      , "Char(str)"
      , "Ident(str)"
      , "Integer(int)"
      , "Double(float)"
      ]
    valueCatsClasses = map createValueCatClass valueCatNames


-- | Converts the production of a define, called an expression, to a
--   production for the parsing definition.
expToDef :: CF -> Exp -> String
expToDef cf (App "(:)" _ (e:[App "[]" _ _])) = expToDef cf e ++ "]"
expToDef cf (App "(:)" _ (e:[recList])) = "[" ++ expToDef cf e ++ ", " ++
  expToDef cf recList
expToDef _ (App "[]" _ _) = "[]"
expToDef cf (App fName _ exps)
  | isLower (head fName) = 
    unkw fName  ++ "(" ++ addCommas (map (expToDef cf) exps) ++ ")"
  | otherwise = 
    unkw fName ++ "(" ++ addCommas (map (expToDef cf) exps) ++ ")"
expToDef _ (Var s) = unkw s
expToDef _ (LitInt i) = "Integer(" ++ show i ++ ")"
expToDef _ (LitDouble d) = "Double(" ++ show d ++ ")"
expToDef _ (LitChar s) = "Char(\"" ++ show s ++ "\")"
expToDef _ (LitString s) = "String('" ++ s ++ "')"


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


-- | The value categories become abstract classes, for type hinting.
createValueCatClass :: String -> String
createValueCatClass s = "class " ++ s ++ ":\n\tpass\n"


-- | Make a Python class from a rule's name and production.
makePythonClass :: Rul RFun -> String
makePythonClass rule = 
  "@dataclass\n" ++
  "class " ++ className ++ ":\n" ++
    if length cats == 0 then "\tpass\n" else classBody
  where
    className = unkw (funName rule)
    sentForm = rhsRule rule
    cats = lefts sentForm
    nvCats = numVars sentForm :: [Either (Cat, Doc) String]

    enumeratedVarsWithType = [render d ++ ": " ++ 
      strCatToPyTyping (show (normCat c)) | (c, d) <- lefts nvCats]

    classBody = unlines $ map ("\t" ++) (enumeratedVarsWithType ++ 
      ["_ann_type: _AnnType = field(default_factory=_AnnType)"])


-- | Creates the corresponding type hinting for some member variable.
strCatToPyTyping :: String -> String
strCatToPyTyping s = if strIsList s 
  then "_List['" ++ (unkw . tail . init) s ++ "']" 
  else unkw s


-- | Creates functions for the defines.
createDefineFunctions :: CF -> String
createDefineFunctions cf = unlines 
  (map (createDefineFunction cf) (definitions cf))


createDefineFunction :: CF -> Define -> String
createDefineFunction cf d = unlines
  [ "def " ++ (unkw . wpThing . defName) d ++ "(" ++ addCommas args ++  "):"
  , "  return " ++ expToDef cf (defBody d)
  ]
  where
    args = map (unkw . fst) (defArgs d)

