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
  runtimeTypeMapping ++ concatMap generateConcreteMapping (zip [1..] rules)
    where
    funs = map fst rules
    runtimeTypeMapping
      | catToStr cat `elem` funs || isList cat = [] -- the category is also a function or a list
      | otherwise = generateRuntimeTypeMapping cat rules


generateRuntimeTypeMapping :: Cat -> [(Fun, [Cat])] -> [String]
generateRuntimeTypeMapping cat rules = 
  let className = cat2DartClassName cat 
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



generateConcreteMapping :: (Int, (Fun, [Cat])) -> [String]
generateConcreteMapping (index, (fun, cats))
  | isNilFun fun || 
    isOneFun fun || 
    isConsFun fun = []  -- these are not represented in the ast
  | otherwise = -- a standard rule
    let 
      className = str2DartClassName fun
      vars = getVars cats
    in 
      generateFunctionHeader className ++ 
      indent 2 (
        [ className ++ "(" ] ++ 
        (indent 1 $ generateArgumentsMapping index vars) ++ 
        [ ");" ]
      )


generateArgumentsMapping :: Int -> [DartVar] -> [String]
generateArgumentsMapping index vars = map convertArgument vars
  where 
    convertArgument var@(vType, _) = 
      let name = buildVariableName var
          field = "ctx.p_" ++ show index ++ "_" ++ "1"
      in name ++ ":" +++ buildArgument vType field
    buildArgument :: DartVarType -> String -> String
    buildArgument (0, typeName) name = 
      "build" ++ upperFirst typeName ++ "(" ++ name ++ "),"
    buildArgument (n, typeName) name = 
      let nextName = "e" ++ show n
          argument = buildArgument (n - 1, typeName) nextName
      in name ++ ".iMap((" ++ nextName ++ ") =>" +++ argument ++ "),"


generateFunctionHeader :: String -> [String]
generateFunctionHeader className = [
    className +++ "build" ++ className ++ "(",
    "  " ++ contextName className +++ "ctx,",
    ") =>"
  ]


contextName :: String -> String
contextName className = className ++ "Context"