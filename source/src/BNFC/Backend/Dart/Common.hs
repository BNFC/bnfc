{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.Common where

import qualified Data.Map as Map
import BNFC.CF
import qualified Data.Char as Char


cat2DartClassName :: Cat -> String
cat2DartClassName cat = str2DartClassName $ identCat $ normCat cat


str2DartClassName :: String -> String
str2DartClassName str = upperFirst $ censorName str


cat2DartType :: Cat -> (Int, String)
cat2DartType cat = toList (0, normCat cat)
  where
    toList :: (Int, Cat) -> (Int, String)
    toList (n, (ListCat name)) = toList (n + 1, name)
    toList (n, name) = (n, (name2DartBuiltIn $ censorName $ catToStr name))


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
-- The amount of nestings, and the underlying type name.
-- Example: List<List<Point>> is (2, Point).
-- This helps to build the AST builder
type DartVarType = (Int, String)


-- The name of a variable.
-- the name generated from the type, 
-- and the number making this variable unique
type DartVarName = (String, Int)


-- Because of the different type representing variables, a different `getVars` is used.
getVars :: [Cat] -> [DartVar]
getVars cats = concatMap mapEntryToVariable $ 
  Map.toList $ 
  foldl countVariables Map.empty $ 
  map toNames cats 
  where
    toNames cat = ((cat2DartType cat), (cat2DartName cat))
    countVariables varsMap entry = 
      let current = Map.findWithDefault 0 entry varsMap
          next = 1 + current
      in Map.insert entry next varsMap
    mapEntryToVariable ((varType, name), amount) 
      | amount <= 1 = [ toDartVar varType name 0 ]
      | otherwise = 
        let variableNameBase = toDartVar varType name
        in map variableNameBase $ [1..amount]
    toDartVar varType name number = (varType, (name, number))


-- From a DartVar build its string representation
buildVariableName :: DartVar -> String
buildVariableName (_, (name, num)) = lowerFirst appendNumber
  where
    appendNumber 
      | num <= 0 = name
      | otherwise = name ++ show num


buildVariableType :: DartVar -> String 
buildVariableType (vType, _) = unpack vType
  where 
    unpack (0, name) = name
    unpack (n, name) = "List<" ++ unpack (n - 1, name) ++ ">"


-- Prevent some type or variable name to be called as some built-in Dart type
censorName :: String -> String
censorName name 
  | name `elem` builtInTypes = "My" ++ upperFirst name
  | otherwise = name
  where
    builtInTypes = [ "int", "double", "String", "bool", "List", "Set", "Map", 
      "Runes", "Symbol", "null", "Null" ]