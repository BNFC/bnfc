{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartBuilder (cf2DartBuilder) where

import BNFC.CF
import BNFC.Backend.Dart.Common
import Data.Maybe      ( mapMaybe )
import BNFC.Utils       ( (+++) )

cf2DartBuilder :: CF -> String
cf2DartBuilder cf = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in 
    unlines $
      imports ++
      helperFunctions ++
      concatMap generateBuilders rules
  where 
    rules  = getAbstractSyntax cf
    imports = [
      "import 'package:antlr4/antlr4.dart';",
      "import 'ast.dart';",
      "import 'stellaParser.dart';  // fix this line depending on where the stellaParser is being lcated" ]
    helperFunctions = [
      "extension IList<E> on List<E> {",
      "  List<T> iMap<T>(T Function(E e) toElement) =>",
      "      map(toElement).toList(growable: false);",
      "}" ]


generateBuilders :: Data -> [String]
generateBuilders (cat, rules) = 
  runtimeTypeMapping ++ concatMap (generateConcreteMapping cat) rules
    where
    funs = map fst rules
    runtimeTypeMapping
      | catToStr cat `elem` funs || isList cat = [] -- the category is also a function or a list
      | otherwise = generateRuntimeTypeMapping cat rules


generateRuntimeTypeMapping :: Cat -> [(Fun, [Cat])] -> [String]
generateRuntimeTypeMapping cat rules = 
  let className = upperFirst $ cat2DartClassName cat 
  in 
    generateFunctionHeader className ++ 
    indent 2 (
      [ "switch (ctx.runtimeType) {" ] ++ 
      (indent 1 $ map buildChild $ map buildClassName rules) ++ 
      [ "};" ]
    )
  where
    buildClassName (fun, _) = str2DartClassName fun
    buildChild name = (contextName name) +++ "c => build" ++ name ++ "(c),"



generateConcreteMapping :: Cat -> (Fun, [Cat]) -> [String]
generateConcreteMapping cat (fun, cats)
  | isNilFun fun || 
    isOneFun fun || 
    isConsFun fun = []  -- these are not represented in the ast
  | otherwise = -- a standard rule
    let 
      className = upperFirst $ cat2DartClassName cat
      vars = getVars cats
    in 
      generateFunctionHeader className ++ 
      indent 2 (
        [ className ++ "(" ] ++ 
        (indent 1 $ generateArgumentsMapping vars) ++ 
        [ ");" ]
      )


generateArgumentsMapping :: [DartVar] -> [String]
generateArgumentsMapping vars = map convertArgument vars
  where 
    convertArgument var@(vType, _) = 
      let name = buildVariableName var
          field = "ctx." ++ name  -- TODO
      in name ++ ":" +++ buildArgument vType field
    buildArgument :: DartVarType -> String -> String
    buildArgument (0, typeName) name = 
      "build" ++ upperFirst typeName ++ "(" ++ name ++ "),"
    buildArgument (n, typeName) name = 
      "name.iMap((e" ++ show n ++ ") =>" +++ buildArgument (n - 1, typeName) name ++ "),"


generateFunctionHeader :: String -> [String]
generateFunctionHeader className = [
    className +++ "build" ++ className ++ "(",
    "  " ++ contextName className +++ "ctx,",
    ") =>"
  ]


contextName :: String -> String
contextName className = className ++ "Context"