{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartAbs (cf2DartAbs) where

import qualified Data.Char as Char
import Data.Maybe      ( mapMaybe )
import qualified Data.Map as Map

import BNFC.CF
import BNFC.Options     ( RecordPositions(..) )
import BNFC.Utils       ( (+++) )

import BNFC.Backend.Common.NamedVariables ( UserDef )

--Produces abstract data types in Dart

-- The type of an instance variable.
-- Variable type, and its name
type DartVar = (String, DartVarName)

-- The name of a variable.
-- the name generated from the type, 
-- and the number making this variable unique
type DartVarName = (String, Int)


cf2DartAbs :: CF -> RecordPositions -> String
cf2DartAbs cf rp = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in unlines $ 
    imports ++  -- import some libraries if needed
    generateTokens userTokens ++  -- generate user-defined types
    concatMap (prData rp) rules
  where
  rules  = getAbstractSyntax cf
  imports = []


generateTokens :: [UserDef] -> [String]
generateTokens tokens = map toClass tokens
  where
    toClass token = 
      let name = censorName token
      in unlines [
        "final class" +++ name +++ "{",  -- A user defined type is a wrapper around the String
        "  final String value;",
        "  const" +++ name ++ "(this.value);",
        "}"
      ]


-- | Generates a (possibly abstract) category class, and classes for all its rules.
prData :: RecordPositions -> Data -> [String]
prData rp (cat, rules) =
  categoryClass ++ mapMaybe (prRule rp cat) rules
    where
    funs = map fst rules
    categoryClass
      | catToStr cat `elem` funs || isList cat = [] -- the category is also a function or a list
      | otherwise = [ "sealed class" +++ cat2DartClassName cat +++ "{}" ]


-- | Generates classes for a rule, depending on what type of rule it is.
prRule :: RecordPositions -> Cat -> (Fun, [Cat]) -> Maybe (String)
prRule rp cat (fun, cats)
  | isNilFun fun || 
    isOneFun fun || 
    isConsFun fun = Nothing  -- these are not represented in the Absyn
  | otherwise = -- a standard rule
    let 
      className = str2DartClassName fun
      vars = getVars cats
    in Just . unlines $ 
      [ unwords [ "class", className, extending, "{" ] ] ++
      concatMap addIndent [
        prInstanceVariables rp vars,
        prConstructor className vars,
        prEquals className vars,
        prHashCode vars
      ] ++ [ "}" ] 
  where
    addIndent line = map ("  " ++) line
    extending 
      | fun == catToStr cat = ""
      | otherwise = "extends" +++ cat2DartClassName cat


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


-- Override the equality `==`
prEquals :: String -> [DartVar] -> [String]
prEquals className variables = [
    "@override",
    "bool operator ==(Object o) =>",
    "  o is" +++ className +++ "&&",
    "  o.runtimeType == runtimeType" ++ 
      (if null variables then ";" else " &&")
  ] ++ checkChildren
  where 
    checkChildren = generateEqualities variables
    generateEqualities [] = []
    generateEqualities (variable:rest) = 
      let name = buildVariableName variable 
      in [ 
        "  " ++ name +++ "==" +++ "o." ++ name ++
          (if null rest then ";" else " &&") 
      ] ++ generateEqualities rest


-- Override the hashCode, combining all instance variables
prHashCode :: [DartVar] -> [String]
prHashCode vars = [
    "@override",
    "int get hashCode => Object.hashAll([" ++ 
      concatMap variableHash vars ++ 
      "]);"
  ] 
  where
    variableHash variable = buildVariableName variable ++ ", "


-- Generate variable definitions for the class
prInstanceVariables :: RecordPositions -> [DartVar] -> [String]
prInstanceVariables rp vars = case rp of 
  RecordPositions -> ["int? line_num, col_num, offset;"] ++ generateVariables
  NoRecordPositions -> generateVariables
  where
    generateVariables = map variableLine vars
    variableLine variable@(varType, _) =
      "final" +++ varType +++ buildVariableName variable ++ ";"
       

-- Generate the class constructor
prConstructor :: String -> [DartVar] -> [String]
prConstructor className vars = 
  [ className ++ "(" ++ variablesAssignment ++ ");" ]
  where 
    variablesAssignment = concatMap assignment vars
    assignment variable = "this." ++ buildVariableName variable ++ ", "


-- From a DartVar build its string representation
buildVariableName :: DartVar -> String
buildVariableName (_, (name, num)) = lowerFirst appendNumber
  where
    appendNumber 
      | num <= 0 = name
      | otherwise = name ++ show num


-- Prevent some type or variable name to be called as some built-in Dart type
censorName :: String -> String
censorName name 
  | name `elem` builtInTypes = "My" ++ upperFirst name
  | otherwise = name
  where
    builtInTypes = [ "int", "double", "String", "bool", "List", "Set", "Map", 
      "Runes", "Symbol", "null", "Null" ]


cat2DartClassName :: Cat -> String
cat2DartClassName cat = str2DartClassName $ identCat $ normCat cat


str2DartClassName :: String -> String
str2DartClassName str = upperFirst $ censorName str


cat2DartType :: Cat -> String
cat2DartType cat = toList $ normCat cat
  where
    toList (ListCat name) = "List<" ++ toList name ++ ">"
    toList name = name2DartBuiltIn $ censorName $ catToStr name


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