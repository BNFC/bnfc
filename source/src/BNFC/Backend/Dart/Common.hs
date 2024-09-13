{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.Common where

import qualified Data.Map as Map
import BNFC.CF
import Data.Maybe
import qualified Data.Char as Char


cat2DartClassName :: String -> Cat -> String
cat2DartClassName langName cat = str2DartClassName langName $ identCat $ normCat cat


-- Pick a class name that is appropriate for the Dart
str2DartClassName :: String -> String -> String
str2DartClassName langName str = upperFirst $ censorName langName str


-- Pick a class name that is appropriate for the Antlr
str2AntlrClassName :: String -> String
str2AntlrClassName str = upperFirst str


cat2DartType :: String -> Cat -> DartVarType
cat2DartType langName cat = toList (0, cat)
  where
    toList :: (Int, Cat) -> DartVarType
    toList (n, (ListCat name)) = toList (n + 1, name)
    toList (n, name) = 
      ( n
      , let n = catToStr $ normCat name
        in case (name2DartBuiltIn n) of 
          Just bn -> bn
          Nothing -> censor n )
    censor = censorName langName


cat2DartName :: String -> Cat -> String
cat2DartName langName cat = toList $ normCat cat
  where
    toList (ListCat name) = toList name ++ "List"
    toList name = censorName langName $ catToStr name


name2DartBuiltIn :: String -> Maybe String
name2DartBuiltIn name
  | name == "Integer" = Just "int"
  | name == "Double" = Just "double"
  | name == "Ident" = Just "String"
  | name == "String" = Just "String"
  | name == "Char" = Just "Character"
  | otherwise = Nothing


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
getVars :: String -> [Cat] -> [DartVar]
getVars langName cats = 
  let variables = map toUnnamedVariable cats 
      namesMap = foldl countNames Map.empty variables
      scoreMap = Map.map addScore namesMap
      (_, vars) = foldl toDartVar (scoreMap, []) variables
  in vars
    where
      cat2DartName' = cat2DartName langName
      cat2DartType' = cat2DartType langName
      toUnnamedVariable cat = ((cat2DartType' cat), (cat2DartName' cat))
      countNames namesMap (_, name) = 
        let current = Map.findWithDefault 0 name namesMap
            next = 1 + current
        in Map.insert name next namesMap
      addScore n = (1, n)
      toDartVar (namesMap, vars) (vType, name) =
        case (Map.lookup name namesMap) of
          Nothing -> (
            namesMap, 
            vars ++ [(vType, (name, 0))])
          Just (seen, total) -> if total <= 1 
            then (
              namesMap, 
              vars ++ [(vType, (name, 0))])
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
    unpack (n, name) = "IList<" ++ unpack (n - 1, name) ++ ">"


checkBuiltIn :: String -> Bool
checkBuiltIn name = 
  (lowerFirst name) `elem` concatMap 
      (map lowerFirst) 
      [ builtIn, keywords ]


checkRegistered :: String -> Bool
checkRegistered name = 
  (lowerFirst name) `elem` concatMap 
      (map lowerFirst) 
      [ builtIn, keywords, taken ]


-- Prevent some type or variable name to be called as some already used type or keyword
censorName :: String -> String -> String
censorName langName name 
  | checkRegistered name = langName ++ upperFirst name
  | otherwise = name

showPrec prec = 
  case prec of 
    0 -> ""
    _ -> show prec

taken = [ "Character" ]

builtIn = [ "int"
          , "double"
          , "num"
          , "String"
          , "bool"
          , "List"
          , "Set"
          , "Map"
          , "Runes"
          , "Symbol"
          , "null"
          , "Null"
          , "Object"
          , "Enum"
          , "Future"
          , "Stream"
          , "Iterable"
          , "Never"
          , "dynamic"
          , "void" ]

keywords = [ "abstract"
          , "as"
          , "assert"
          , "async"
          , "await"
          , "base"
          , "break"
          , "case"
          , "catch"
          , "class"
          , "const"
          , "continue"
          , "covariant"
          , "default"
          , "deferred"
          , "do"
          , "dynamic"
          , "else"
          , "enum"
          , "export"
          , "extends"
          , "extension"
          , "external"
          , "factory"
          , "false"
          , "final"
          , "finally"
          , "for"
          , "Function"
          , "get"
          , "hide"
          , "if"
          , "implements"
          , "import"
          , "in"
          , "interface"
          , "is"
          , "late"
          , "library"
          , "mixin"
          , "new"
          , "null"
          , "of"
          , "on"
          , "operator"
          , "part"
          , "required"
          , "rethrow"
          , "return"
          , "sealed"
          , "set"
          , "show"
          , "static"
          , "super"
          , "switch"
          , "sync"
          , "this"
          , "throw"
          , "true"
          , "try"
          , "type"
          , "typedef"
          , "var"
          , "void"
          , "when"
          , "with"
          , "while"
          , "yield" ]