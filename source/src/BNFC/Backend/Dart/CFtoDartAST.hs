{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartAST (cf2DartAST) where

import Data.Maybe      ( mapMaybe )

import BNFC.CF
import BNFC.Utils       ( (+++) )

import BNFC.Backend.Common.NamedVariables ( UserDef )
import BNFC.Backend.Dart.Common 

-- Produces abstract data types in Dart
cf2DartAST :: String -> CF -> String
cf2DartAST langName cf = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in unlines 
    $ imports -- import some libraries if needed
    ++ characterTypedef
    ++ generateTokens userTokens 
    ++ concatMap prData rules  -- generate user-defined types
  where
    rules  = getAbstractSyntax cf
    imports = 
      [ "import 'pretty_printer.dart' as pp;"
      , "import 'package:fast_immutable_collections/fast_immutable_collections.dart';" ]
    characterTypedef = [ "typedef Character = String;" ]
    censorName' = censorName langName
    str2DartClassName' = str2DartClassName langName
    cat2DartClassName' = cat2DartClassName langName
    getVars' = getVars langName


    generateTokens :: [UserDef] -> [String]
    generateTokens = map $ \token -> 
        let name = censorName' token 
        in "typedef" +++ name +++ "= String;"
          

    -- | Generates a category class, and classes for all its rules.
    prData :: Data -> [String]
    prData (cat, rules) =
      categoryClass ++ mapMaybe (prRule cat) rules
        where
        funs = map fst rules
        categoryClass
          | catToStr cat `elem` funs || isList cat = [] -- the category is also a function or a list
          | otherwise =
            let name = cat2DartClassName' cat
            in 
              [ "sealed class" +++ name +++ "with pp.Printable {"
              , "  @override"
              , "  String get print => pp.print" ++ name ++ "(this);"
              , "}" ]


    -- | Generates classes for a rule, depending on what type of rule it is.
    prRule :: Cat -> (Fun, [Cat]) -> Maybe (String)
    prRule cat (fun, cats)
      | isNilFun fun || 
        isOneFun fun || 
        isConsFun fun = Nothing  -- these are not represented in the Absyn
      | otherwise = -- a standard rule
        let 
          className = str2DartClassName' fun
          vars = getVars' cats
        in Just . unlines $ 
          [ unwords [ "class", className, extending, "with pp.Printable {" ] ] ++
          concatMap (indent 1) [
            prInstanceVariables vars,
            prConstructor className vars,
            prEquals className vars,
            prHashCode vars,
            prPrettyPrint className
          ] ++ [ "}" ] 
      where
        extending 
          | fun == catToStr cat = ""
          | otherwise = "extends" +++ cat2DartClassName' cat


    -- Override the equality `==`
    prEquals :: String -> [DartVar] -> [String]
    prEquals className variables = 
      [ "@override"
      , "bool operator ==(Object o) =>"
      , "  o is" +++ className +++ "&&"
      , "  o.runtimeType == runtimeType" 
        ++ ( case variables of 
          [] -> ";" 
          _ -> " &&" )
      ] ++ checkChildren
      where 
        checkChildren = buildLines $ map (eqCond . buildVariableName) variables 
        eqCond name = "  " ++ name +++ "==" +++ "o." ++ name 
        buildLines [] = []
        buildLines [x] = [x ++ ";"]
        buildLines (x:xs) = [x ++ " &&"] ++ (buildLines xs)


    -- Override the hashCode, combining all instance variables
    prHashCode :: [DartVar] -> [String]
    prHashCode vars = 
      [ "@override"
      , "int get hashCode => Object.hashAll([" 
        ++ (concatMap ((++ ", ") . buildVariableName) vars) 
        ++ "]);" ] 


    -- Generate variable definitions for the class
    prInstanceVariables :: [DartVar] -> [String]
    prInstanceVariables vars = map variableAssignment vars
      where
        variableAssignment v =  
          let vType = buildVariableType v
              vName = buildVariableName v
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
    prPrettyPrint name = 
      [ "@override"
      , "String get print => pp.print" ++ name ++ "(this);" ]
