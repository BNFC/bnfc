{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartAST (cf2DartAST) where

import Data.Maybe      ( mapMaybe )

import BNFC.CF
import BNFC.Options     ( RecordPositions(..) )
import BNFC.Utils       ( (+++) )

import BNFC.Backend.Common.NamedVariables ( UserDef )
import BNFC.Backend.Dart.Common 

-- Produces abstract data types in Dart
cf2DartAST :: CF -> RecordPositions -> String
cf2DartAST cf rp = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in unlines $ 
    imports ++  -- import some libraries if needed
    generateTokens userTokens ++  -- generate user-defined types
    concatMap (prData rp) rules
  where
  rules  = getAbstractSyntax cf
  imports = [ 
    "import 'pretty_printer.dart' as pp;",
    "import 'package:fast_immutable_collections/fast_immutable_collections.dart';" ]


generateTokens :: [UserDef] -> [String]
generateTokens tokens = map toClass tokens
  where
    toClass token = 
      let name = censorName token
      in unlines [
        "final class" +++ name +++ "with pp.Printable {",  -- A user defined type is a wrapper around the String
        "  final String value;",
        "  const" +++ name ++ "(this.value);",
        "",
        "  @override",
        "  String get print => pp.print" ++ name ++ "(this);",
        "}"
      ]


-- | Generates a category class, and classes for all its rules.
prData :: RecordPositions -> Data -> [String]
prData rp (cat, rules) =
  categoryClass ++ mapMaybe (prRule rp cat) rules
    where
    funs = map fst rules
    categoryClass
      | catToStr cat `elem` funs || isList cat = [] -- the category is also a function or a list
      | otherwise =
        let name = cat2DartClassName cat
        in [ 
          "sealed class" +++ name +++ "with pp.Printable {",
          "  @override",
          "  String get print => pp.print" ++ name ++ "(this);",
          "}" ]


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
      [ unwords [ "class", className, extending, "with pp.Printable {" ] ] ++
      concatMap (indent 1) [
        prInstanceVariables rp vars,
        prConstructor className vars,
        prEquals className vars,
        prHashCode vars,
        prPrettyPrint className
      ] ++ [ "}" ] 
  where
    extending 
      | fun == catToStr cat = ""
      | otherwise = "extends" +++ cat2DartClassName cat


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
    variableLine variable =
      let vType = buildVariableType variable
          vName = buildVariableName variable
      in "final" +++ vType +++ vName ++ ";"
       

-- Generate the class constructor
prConstructor :: String -> [DartVar] -> [String]
prConstructor className vars = 
  [ className ++ "(" ++ variablesAssignment ++ ");" ]
  where 
    variablesAssignment
      | null vars = ""
      | otherwise = "{" ++ (concatMap assignment vars) ++ "}"
    assignment variable = "required this." ++ buildVariableName variable ++ ", "

prPrettyPrint :: String -> [String]
prPrettyPrint name = [
  "@override",
  "String get print => pp.print" ++ name ++ "(this);" ]
