{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartBuilder (cf2DartBuilder) where

import BNFC.CF
import BNFC.Backend.Dart.Common
import Data.Maybe      ( mapMaybe )
import BNFC.Utils       ( (+++) )
import Data.List ( intercalate )

cf2DartBuilder :: CF -> String
cf2DartBuilder cf = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in 
    unlines $
      imports ++
      helperFunctions ++
      concatMap generateBuilders rules
  where 
    rules = ruleGroups cf
      -- getAbstractSyntax cf
    imports = [
      "import 'package:antlr4/antlr4.dart';",
      "import 'ast.dart';",
      "import 'stellaParser.dart';  // fix this line depending on where the stellaParser is being lcated" ]
    helperFunctions = [
      "extension IList<E> on List<E> {",
      "  List<T> iMap<T>(T Function(E e) toElement) =>",
      "      map(toElement).toList(growable: false);",
      "}" ]


generateBuilders :: (Cat, [Rule]) -> [String]
generateBuilders (cat, rawRules) = 
  let 
    rules = map reformatRule rawRules
    funs = map fst rules
  in
    runtimeTypeMapping funs rules ++ concatMap concreteMapping (zip [1..] rawRules)
  where
    
    -- funs = map funRule rawRules
    -- cats = map 
    -- runtimeTypeMapping = generateRuntimeTypeMapping cat rules
    runtimeTypeMapping funs rules
      | isList cat || catToStr cat `elem` funs = [] -- the category is also a function or a list
      | otherwise = generateRuntimeTypeMapping cat rules
    concreteMapping (index, rule) = generateConcreteMapping index rule


reformatRule :: Rule -> (String, [Cat])
reformatRule rule = (wpThing $ funRule rule, [normCat c | Left c <- rhsRule rule ])


generateRuntimeTypeMapping :: Cat -> [(String, [Cat])] -> [String]
generateRuntimeTypeMapping cat rules = 
  let className = cat2DartClassName cat 
  in [  
    className ++ "?" +++ "build" ++ className ++ "(" ++ contextName className ++ "?" +++ "ctx" ++ ") =>" 
  ] ++ indent 1 (
    [ "switch (ctx?.runtimeType) {" ] ++ 
    (indent 1 $ addDefaultCase $ map buildChild $ map buildClassName rules) ++ 
    [ "};" ]
  )
  where
    buildClassName (fun, _) = str2DartClassName fun
    buildChild name = (contextName name) +++ "c => build" ++ name ++ "(c),"
    addDefaultCase cases = cases ++ [ "_ => null," ]


generateConcreteMapping :: Int -> Rule -> [String]
generateConcreteMapping index rule = 
  generateConcreteMappingHelper index rule $ reformatRule rule


generateConcreteMappingHelper :: Int -> Rule -> (String, [Cat]) -> [String]
generateConcreteMappingHelper index rule (fun, cats)
  | isNilFun fun || 
    isOneFun fun || 
    isConsFun fun = []  -- these are not represented in the ast
  | otherwise = -- a standard rule
    let 
      className = str2DartClassName fun
      vars = getVars cats
    in [
      className ++ "?" +++ "build" ++ className ++ "(" ++ contextName className ++ "?" +++ "ctx) {"
    ] ++ (
      indent 1 $ 
        (generateArguments index rule vars) ++ 
        (generateNullCheck vars) ++ 
        [ "return" +++ className ++ "(" ]
    ) ++ (
      indent 2 $ generateArgumentsMapping vars 
    ) ++ indent 1 [
      ");"
    ] ++ [
      "}"
    ]
      

generateArguments :: Int -> Rule -> [DartVar] -> [String]
generateArguments index r vars = 
  case rhsRule r of
    [] -> []
    its -> traverseRule index 1 its vars []


traverseRule :: Int -> Int -> [Either Cat String] -> [DartVar] -> [String] -> [String]
traverseRule _ _ _ [] lines = lines
traverseRule _ _ [] _ lines = lines
traverseRule ind1 ind2 (terminal:restTerminals) (variable@(vType, _):restVariables) lines = 
  case terminal of 
    Left cat -> traverseRule ind1 (ind2 + 1) restTerminals restVariables lines ++ [
      "final" +++ buildVariableName variable +++ "=" +++ buildArgument vType field ++ ";" ] 
    Right _ -> traverseRule ind1 (ind2 + 1) restTerminals (variable:restVariables) lines
  where
    field = "ctx?.p_" ++ show ind1 ++ "_" ++ show ind2
    buildArgument :: DartVarType -> String -> String
    buildArgument (0, typeName) name = 
      "build" ++ upperFirst typeName ++ "(" ++ name ++ ")"
    buildArgument (n, typeName) name = 
      let nextName = "e" ++ show n
          argument = buildArgument (n - 1, typeName) nextName
      in name ++ "?.iMap((" ++ nextName ++ ") =>" +++ argument ++ ")"


generateNullCheck :: [DartVar] -> [String]
generateNullCheck [] = []
generateNullCheck vars = 
  [ "if (" ] ++ 
  (indent 1 [ intercalate " || " $ map condition vars ]) ++
  [ ") {" ] ++
  (indent 1 [ "return null;" ]) ++
  [ "}" ]
  where
    condition :: DartVar -> String
    condition var = buildVariableName var +++ "==" +++ "null"


generateArgumentsMapping :: [DartVar] -> [String]
generateArgumentsMapping vars = map mapArgument vars
  where
    mapArgument variable = 
      let name = buildVariableName variable
      in name ++ ":" +++ name ++ ","


contextName :: String -> String
contextName className = className ++ "Context"