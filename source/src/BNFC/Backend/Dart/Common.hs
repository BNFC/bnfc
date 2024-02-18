{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.Common where

import qualified Data.Map as Map
import BNFC.CF
import qualified Data.Char as Char


cat2DartClassName :: Cat -> String
cat2DartClassName cat = str2DartClassName $ identCat $ normCat cat


-- Pick a class name that is appropriate for the Dart
str2DartClassName :: String -> String
str2DartClassName str = upperFirst $ censorName str


-- Pick a class name that is appropriate for the Antlr
str2AntlrClassName :: String -> String
str2AntlrClassName str = upperFirst str


cat2DartType :: Cat -> DartVarType
cat2DartType cat = toList (0, cat)
  where
    toList :: (Int, Cat) -> DartVarType
    toList (n, (ListCat name)) = toList (n + 1, name)
    toList (n, name) = (n, (name2DartBuiltIn $ catToStr $ normCat name))


cat2DartName :: Cat -> String
cat2DartName cat = toList $ normCat cat
  where
    toList (ListCat name) = toList name ++ "List"
    toList name = censorName $ catToStr name


name2DartBuiltIn :: String -> String
name2DartBuiltIn name
  | name == "Integer" = "int"
  | name == "Double" = "double"
  | name == "Ident" = "String"
  | name == "Char" = "String" -- TODO
  | otherwise = name


upperFirst :: [Char] -> [Char]
upperFirst [] = []
upperFirst (letter:rest) = Char.toUpper letter : rest


lowerFirst :: [Char] -> [Char]
lowerFirst [] = []
lowerFirst (letter:rest) = Char.toLower letter : rest


indent :: Int -> [String] -> [String]
indent n lines = map addSpaces lines
  where
    addSpaces :: String -> String
    addSpaces line = (replicate (2 * n) ' ') ++ line


-- The type of an instance variable.
-- Variable type, and its name
type DartVar = (DartVarType, DartVarName)


-- The type of a variable type in Dart.
-- The amount of nestings, and the underlying type name without precedence.
-- Example: List<List<Expr1>> is (2, Expr).
-- This helps to build the AST builder
type DartVarType = (Int, String)


-- The name of a variable.
-- the name generated from the type, 
-- and the number making this variable unique
type DartVarName = (String, Int)


-- Because of the different type representing variables, a different `getVars` is used.
getVars :: [Cat] -> [DartVar]
getVars cats = 
  let variables = map toUnnamedVariable cats 
      namesMap = foldl countNames Map.empty variables
      scoreMap = Map.map addScore namesMap
      (_, vars) = foldl toDartVar (scoreMap, []) variables
  in vars
  where
    toUnnamedVariable cat = ((cat2DartType cat), (cat2DartName cat))
    countNames namesMap (_, name) = 
      let current = Map.findWithDefault 0 name namesMap
          next = 1 + current
      in Map.insert name next namesMap
    addScore n = (1, n)
    toDartVar (namesMap, vars) (vType, name) =
      case (Map.lookup name namesMap) of
        Nothing            -> (namesMap, vars ++ [(vType, (name, 0))])
        Just (seen, total) -> if total <= 1 
          then (namesMap, vars ++ [(vType, (name, 0))])
          else (
            Map.insert name (seen + 1, total) namesMap, 
            vars ++ [(vType, (name, seen))])


-- From a DartVar build its string representation
buildVariableName :: DartVar -> String
buildVariableName (_, (name, num)) = lowerFirst appendNumber
  where
    appendNumber 
      | num <= 0 = name
      | otherwise = name ++ show num


-- From a DartVar make a name for the AST
buildVariableType :: DartVar -> String 
buildVariableType (vType, _) = buildVariableTypeFromDartType vType
  
buildVariableTypeFromDartType :: DartVarType -> String
buildVariableTypeFromDartType vType = unpack vType
  where 
    unpack (0, name) = name
    unpack (n, name) = "List<" ++ unpack (n - 1, name) ++ ">"


-- Prevent some type or variable name to be called as some built-in Dart type
censorName :: String -> String
censorName name 
  | (lowerFirst name) `elem` (map lowerFirst builtInTypes) = "My" ++ upperFirst name
  | otherwise = name
  where
    builtInTypes = [ "int", "double", "String", "bool", "List", "Set", "Map", 
      "Runes", "Symbol", "null", "Null" ]