{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartBuilder (cf2DartBuilder) where

import BNFC.CF
import BNFC.Backend.Dart.Common
import BNFC.Utils       ( (+++) )
import Data.List ( intercalate, find )
import Data.Either ( isLeft )

cf2DartBuilder :: CF -> String
cf2DartBuilder cf = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in 
    unlines $
      imports ++
      helperFunctions ++
      map buildUserToken userTokens ++
      concatMap generateBuilders rules
  where 
    rules = ruleGroups cf
    imports = [
      "import 'package:antlr4/antlr4.dart';",
      "import 'ast.dart';",
      "import 'stellaParser.dart';  // fix this line depending on where the stellaParser is being lcated" ]
    helperFunctions = [
      "extension IList<E> on List<E> {",
      "  List<T> iMap<T>(T Function(E e) toElement) =>",
      "      map(toElement).toList(growable: false);",
      "}",
      "int? buildInt(Token? t) => t?.text != null ? int.tryParse(t!.text!) : null;",
      "double? buildDouble(Token? t) => t?.text != null ? double.tryParse(t!.text!) : null;",
      "String? buildString(Token? t) => t?.text;" ]
    buildUserToken token =
      let name = censorName token
      in token ++ "? build" ++ token ++ "(Token? t) {\n" ++
        "  final text = t?.text;\n" ++
        "  return text != null ?" +++ token ++ "(text) : null;\n}"


generateBuilders :: (Cat, [Rule]) -> [String]
generateBuilders (cat, rawRules) = 
  let 
    numeratedRawRules = zip [1..] rawRules
  in 
    runtimeTypeMapping numeratedRawRules ++ 
    concatMap (\(index, rule) -> generateConcreteMapping index rule) numeratedRawRules
  where
    funs numeratedRawRules = (map (\(_, rule) -> wpThing $ funRule rule) numeratedRawRules)
    runtimeTypeMapping numeratedRawRules
      | isList cat || 
        (catToStr cat) `elem` (funs numeratedRawRules) = [] -- the category is also a function or a list
      | otherwise = generateRuntimeTypeMapping cat [
        (index, wpThing $ funRule rule, rhsRule rule) | 
        (index, rule) <- numeratedRawRules ]


-- TODO get rid of this reformating and pass the actual sturcture everywhere
reformatRule :: Rule -> (String, [Cat])
reformatRule rule = (wpThing $ funRule rule, [normCat c | Left c <- rhsRule rule ])


generateRuntimeTypeMapping :: Cat -> [(Int, String, [Either Cat String])] -> [String]
generateRuntimeTypeMapping cat rules = 
  let astName = cat2DartClassName cat 
      prec = precCat cat
      precedencedName = astName ++ (if prec == 0 then "" else show prec)
  in [  
    astName ++ "?" +++ "build" ++ precedencedName ++ "(" ++ contextName precedencedName ++ "?" +++ "ctx" ++ ") =>" 
  ] ++ indent 1 (
    [ "switch (ctx?.runtimeType) {" ] ++ 
    (indent 1 $ addDefaultCase $ map (buildChild precedencedName) rules) ++ 
    [ "};" ]
  )
  where
    -- TODO FIX make this synchronized with the parser generator
    -- TODO one antlr context class may have multiple arguments from different rules
    buildUniversalChild name fun arg = name +++ "c => build" ++ fun ++ "(" ++ arg ++ "),"
    buildChild className (index, name, rhs)
      | isNilFun name = 
        buildUniversalChild (contextName className ++ "_Empty") className "c"
      | isOneFun name = 
        buildUniversalChild (contextName className ++ "_AppendLast") className "c"
      | isConsFun name = 
        buildUniversalChild (contextName className ++ "_PrependFirst") className "c"
      | isCoercion name = 
        let 
          (coercionType, ind2) = case (find (\(_, value) -> isLeft value) $ zip [1..] rhs) of 
            Just (i, Left cat) -> (
              let prec = precCat cat in (cat2DartClassName cat) ++ (if prec == 0 then "" else show prec), 
              show i )
            otherwise -> (className, "") -- error, no category for the coercion
          lineIndex = show index
          argument = "p_" ++ lineIndex ++ "_" ++ ind2
        in 
          buildUniversalChild ("Coercion_" ++ contextName (className ++ "_" ++ lineIndex)) coercionType ("c." ++ argument)
      | otherwise = 
        buildUniversalChild (contextName $ str2AntlrClassName name) (str2DartClassName name) "c"
    addDefaultCase cases = cases ++ [ "_ => null," ]


generateConcreteMapping :: Int -> Rule -> [String]
generateConcreteMapping index rule = 
  generateConcreteMappingHelper index rule $ reformatRule rule


generateConcreteMappingHelper :: Int -> Rule -> (String, [Cat]) -> [String]
generateConcreteMappingHelper index rule (fun, cats)
  | isCoercion fun ||
    isNilFun fun || 
    isOneFun fun || 
    isConsFun fun = []  -- these are not represented in the ast
  | otherwise = -- a standard rule
    let 
      className = str2DartClassName fun
      antlrContextName = contextName $ str2AntlrClassName fun
      vars = getVars cats
    in [
      className ++ "?" +++ "build" ++ className ++ "(" ++ antlrContextName ++ "?" +++ "ctx) {"
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
    Left cat -> [
        "final" +++ buildVariableName variable +++ "=" +++ buildArgument (precCat cat) vType field ++ ";" 
      ] ++ traverseRule ind1 (ind2 + 1) restTerminals restVariables lines
    Right _ -> traverseRule ind1 (ind2 + 1) restTerminals (variable:restVariables) lines
  where
    field = "ctx?.p_" ++ show ind1 ++ "_" ++ show ind2
    buildArgument :: Integer -> DartVarType -> String -> String
    buildArgument prec (0, typeName) name = 
      let precedence = if prec == 0 then "" else show prec
      in "build" ++ upperFirst typeName ++ precedence ++ "(" ++ name ++ ")"
    buildArgument prec (n, typeName) name = 
      let nextName = "e" ++ show n
          argument = buildArgument prec (n - 1, typeName) nextName
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
