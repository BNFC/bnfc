{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartBuilder (cf2DartBuilder) where

import BNFC.CF
import BNFC.Backend.Dart.Common
import BNFC.Backend.Antlr.CFtoAntlr4Parser (makeLeftRecRule)
import BNFC.Utils       ( (+++) )
import Data.List ( intercalate, find )
import Data.Either ( isLeft )

cf2DartBuilder :: CF -> String -> String
cf2DartBuilder cf lang = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in 
    unlines $
      imports lang ++
      helperFunctions ++
      map buildUserToken userTokens ++
      concatMap generateBuilders rules
  where 
    leftRecRuleMaker = (makeLeftRecRule cf)
    rules = map 
      (\(cat, rules) -> (cat, (map leftRecRuleMaker rules))) $ ruleGroups cf
    imports lang = [
      "import 'package:antlr4/antlr4.dart';",
      "import 'package:fast_immutable_collections/fast_immutable_collections.dart' show IList;",
      "import 'ast.dart';",
      "import '" ++ lang ++ "Parser.dart';  // fix this line depending on where the stellaParser is being lcated" ]
    helperFunctions = [
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
      | (catToStr cat) `elem` (funs numeratedRawRules) = [] -- the category is also a function or a list
      | otherwise = generateRuntimeTypeMapping cat [
        (index, wpThing $ funRule rule, rhsRule rule) | 
        (index, rule) <- numeratedRawRules ]


reformatRule :: Rule -> (String, [Cat])
reformatRule rule = (wpThing $ funRule rule, [normCat c | Left c <- rhsRule rule ])


generateRuntimeTypeMapping :: Cat -> [(Int, String, [Either Cat String])] -> [String]
generateRuntimeTypeMapping cat rules = 
  let ctxName = cat2DartClassName cat 
      astName = buildVariableTypeFromDartType $ cat2DartType cat 
      prec = precCat cat
      precedencedName = ctxName ++ (if prec == 0 then "" else show prec)
  in [  
    astName ++ "?" +++ "build" ++ precedencedName ++ "(" ++ (contextName precedencedName) ++ "?" +++ "ctx" ++ ") {" 
  ] ++ indent 1 (
    (map (buildChild precedencedName) rules) ++
    ["return null;"]
  ) ++ ["}"]
  where
    buildUniversalChild name fun arg = 
      "if (ctx is" +++ name ++ ") return build" ++ fun ++ "(" ++ arg ++ ");"
    buildChild className (index, name, rhs) = case (antlrListSuffix name) of
      "" -> if (isCoercion name)
        then 
          let (coercionType, ind2) = case (find (\(_, value) -> isLeft value) $ zip [1..] rhs) of 
                Just (i, Left cat) -> (
                  let prec = precCat cat in (cat2DartClassName cat) ++ (if prec == 0 then "" else show prec), 
                  show i )
                otherwise -> (className, "") -- error, no category for the coercion
              lineIndex = show index
              argument = "p_" ++ lineIndex ++ "_" ++ ind2
          in 
            buildUniversalChild ("Coercion_" ++ contextName (className ++ "_" ++ lineIndex)) coercionType ("ctx." ++ argument)
        else 
          buildUniversalChild (contextName $ str2AntlrClassName name) (str2DartClassName name) "ctx"
      suffix -> 
        buildUniversalChild (contextName (className ++ "_" ++ suffix)) (className ++ suffix) "ctx"


generateConcreteMapping :: Int -> Rule -> [String]
generateConcreteMapping index rule = 
  generateConcreteMappingHelper index rule $ reformatRule rule


generateConcreteMappingHelper :: Int -> Rule -> (String, [Cat]) -> [String]
generateConcreteMappingHelper index rule (fun, cats)
  | isCoercion fun = []
  | otherwise =
    let 
      (typeName, className, ctxName) = 
        if (isNilFun fun ||
            isOneFun fun ||
            isConsFun fun)
        then 
          let cat = valCat rule
              prec = case (precCat cat) of 
                0 -> ""
                i -> show i
              ctxName = (cat2DartClassName cat) ++ prec
              suffix = antlrListSuffix fun
              precedencedName = ctxName ++ suffix
              suffixedCtxName = contextName (ctxName ++ "_" ++ suffix)
              astName = buildVariableTypeFromDartType $ cat2DartType cat
          in (astName, precedencedName, suffixedCtxName)
        else 
          let name = str2DartClassName fun
              ctxName = contextName $ str2AntlrClassName fun
          in (name, name, ctxName)
      vars = getVars cats
    in [
      typeName ++ "?" +++ "build" ++ className ++ "(" ++ ctxName ++ "?" +++ "ctx) {"
    ] ++ (
      indent 1 $ 
        (generateArguments index rule vars) ++ 
        (generateNullCheck vars) ++ 
        (generateReturnStatement fun vars typeName)
    ) ++ [
      "}"
    ]
  where
    generateReturnStatement :: Fun -> [DartVar] -> String -> [String]
    generateReturnStatement fun vars typeName
      | isNilFun fun = ["return IList();"]
      | isOneFun fun = generateOneArgumentListReturn vars
      | isConsFun fun = generateTwoArgumentsListReturn vars
      | otherwise =  [ "return" +++ typeName ++ "(" ] ++
        (indent 1 $ generateArgumentsMapping vars ) ++ [");"] 
      

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
    buildArgument prec (_, typeName) name = 
      let precedence = if prec == 0 then "" else show prec
      in "buildList" ++ upperFirst typeName ++ precedence ++ "(" ++ name ++ ")"


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


generateOneArgumentListReturn :: [DartVar] -> [String]
generateOneArgumentListReturn (v:_) = 
  ["return IList([" ++ buildVariableName v ++ "]);"]


generateTwoArgumentsListReturn :: [DartVar] -> [String]
generateTwoArgumentsListReturn (x:y:_) = 
  let (a, b) = putListSecond x y
  in ["return IList([" ++ buildVariableName a ++ ", ..." ++ buildVariableName b ++ ",]);"]
  where 
    putListSecond x@((0,_),_) y = (x, y)
    putListSecond x y = (y, x)


contextName :: String -> String
contextName className = className ++ "Context"


antlrListSuffix :: Fun -> String
antlrListSuffix fun
  | isNilFun fun = "Empty"
  | isOneFun fun = "AppendLast"
  | isConsFun fun = "PrependFirst"
  | otherwise = ""