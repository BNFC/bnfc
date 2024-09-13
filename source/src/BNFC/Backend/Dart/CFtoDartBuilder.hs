{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartBuilder (cf2DartBuilder) where

import BNFC.CF
import BNFC.Backend.Common.NamedVariables (firstLowerCase) 
import BNFC.Backend.Dart.Common
import BNFC.Backend.Antlr.CFtoAntlr4Parser (makeLeftRecRule)
import BNFC.Utils       ( (+++) )
import Data.List ( intercalate, find )
import Data.Either ( isLeft )

cf2DartBuilder :: String -> CF -> String
cf2DartBuilder lang cf = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in 
    unlines $
      imports lang ++
      helperFunctions ++
      map buildUserToken userTokens ++
      concatMap generateBuilders rules
  where 
    leftRecRuleMaker = (makeLeftRecRule cf)
    rules = map mkRule $ ruleGroups cf
    mkRule cat rules = (cat, (map leftRecRuleMaker rules))
    imports lang = 
      [ "import 'package:antlr4/antlr4.dart' show Token;"
      , "import 'package:fast_immutable_collections/fast_immutable_collections.dart' show IList;"
      , "import 'ast.dart';"
      , "import '" ++ (firstLowerCase lang) ++ "_parser.dart';  // fix this line depending on where the stellaParser is being lcated" ]
    helperFunctions = 
      [ "int? buildInteger(Token? t) => t?.text != null ? int.tryParse(t!.text!) : null;"
      , "double? buildDouble(Token? t) => t?.text != null ? double.tryParse(t!.text!) : null;"
      , "String? buildString(Token? t) => t?.text;"
      , "String? buildChar(Token? t) => t?.text;"
      , "String? buildIdent(Token? t) => t?.text;" ]
    buildUserToken token = 
      let name = censorName lang token
      in name ++ "? build" ++ name ++ "(Token? t) => t?.text;"
    str2DartClassName' = str2DartClassName lang
    getVars' = getVars lang
    cat2DartClassName' = cat2DartClassName lang
    cat2DartType' = cat2DartType lang

    generateBuilders :: (Cat, [Rule]) -> [String]
    generateBuilders (cat, rawRules) = 
      let 
        numeratedRawRules = zip [1..] rawRules
      in runtimeTypeMapping numeratedRawRules
        ++ (concatMap (uncurry generateConcreteMapping) numeratedRawRules)
      where
        funsFrom = map (\(_, rule) -> wpThing $ funRule rule)
        runtimeTypeMapping numeratedRawRules
          | (catToStr cat) `elem` (funsFrom numeratedRawRules) = []
          | otherwise = generateRuntimeTypeMapping cat [
            (index, wpThing $ funRule rule, rhsRule rule) | 
            (index, rule) <- numeratedRawRules ]


    reformatRule :: Rule -> (String, [Cat])
    reformatRule rule = 
      (wpThing $ funRule rule, [normCat c | Left c <- rhsRule rule ])


    generateRuntimeTypeMapping :: Cat -> [(Int, String, [Either Cat String])] -> [String]
    generateRuntimeTypeMapping cat rules = 
      let ctxName = upperFirst $ identCat $ normCat cat
          astName = buildVariableTypeFromDartType $ cat2DartType' cat 
          prec = case precCat cat of 
            0 -> ""
            x -> show x
          precedencedName = ctxName ++ prec
      in 
        [ astName ++ "?" +++ "build" ++ precedencedName ++ "(" 
        ++ (contextName precedencedName) ++ "?" +++ "ctx" ++ ") {" ] 
        ++ indent 1 ( (map (buildChild precedencedName) rules) 
          ++ ["return null;"] ) 
        ++ [ "}" ]
      where
        buildUniversalChild name fun arg = 
          "if (ctx is" +++ name ++ ") return build" ++ fun ++ "(" ++ arg ++ ");"
        buildChild className (index, name, rhs) = case (antlrListSuffix name) of
          "" -> if (isCoercion name)
              then 
                let firstCat = find 
                        (\(_, value) -> isLeft value) 
                        $ zip [1..] rhs
                    (coercionType, ind2) = case (firstCat) of 
                        Just (i, Left cat) -> 
                          ( let precStr = case precCat cat of 
                                  0 -> ""
                                  x -> show x
                                catName = upperFirst $ identCat $ normCat cat
                            in catName ++ precStr
                          , show i )
                        otherwise -> (className, "") -- error, no category in the coercion rule
                    lineIndex = show index
                    argument = "p_" ++ lineIndex ++ "_" ++ ind2
                in 
                  buildUniversalChild 
                    ("Coercion_" ++ contextName (className ++ "_" ++ lineIndex)) 
                    coercionType 
                    ("ctx." ++ argument)
              else 
                buildUniversalChild 
                  (contextName $ str2AntlrClassName name) 
                  name
                  "ctx"
          suffix -> buildUniversalChild 
              (contextName (className ++ "_" ++ suffix)) 
              (className ++ suffix) 
              "ctx"


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
                  ctxName = (++ prec) $ upperFirst $ identCat $ normCat cat
                  suffix = antlrListSuffix fun
                  precedencedName = ctxName ++ suffix
                  suffixedCtxName = contextName (ctxName ++ "_" ++ suffix)
                  astName = buildVariableTypeFromDartType $ cat2DartType' cat
              in (astName, precedencedName, suffixedCtxName)
            else 
              let name = str2DartClassName' fun
                  ctxName = contextName $ str2AntlrClassName fun
              in (name, fun, ctxName)
          vars = getVars' cats
        in [
          typeName ++ "?" +++ "build" ++ className ++ "(" ++ ctxName ++ "?" +++ "ctx) {"
        ] ++ (
          indent 1 $ 
            (generateArguments index rule $ zip vars cats) ++ 
            (generateNullCheck vars) ++ 
            (generateReturnStatement fun vars typeName)
        ) ++ [
          "}"
        ]
      where
        generateReturnStatement :: Fun -> [DartVar] -> String -> [String]
        generateReturnStatement fun vars typeName
          | isNilFun fun = [ "return IList();" ]
          | isOneFun fun = generateOneArgumentListReturn vars
          | isConsFun fun = generateTwoArgumentsListReturn vars
          | otherwise =  [ "return" +++ typeName ++ "(" ] 
            ++ (indent 1 $ generateArgumentsMapping vars ) ++ [ ");" ] 
          

    generateArguments :: Int -> Rule -> [(DartVar, Cat)] -> [String]
    generateArguments index r vars = 
      case rhsRule r of
        [] -> []
        its -> traverseRule index 1 its vars []


    traverseRule :: Int -> Int -> [Either Cat String] -> [(DartVar, Cat)] -> [String] -> [String]
    traverseRule _ _ _ [] lines = lines
    traverseRule _ _ [] _ lines = lines
    traverseRule ind1 ind2 (terminal:restTs) (var@(varDart, varCat):restVars) lines = 
      case terminal of 
        Left cat -> 
          let lhs = buildVariableName varDart
              rhs = buildArgument 
                  (precCat cat) 
                  (upperFirst $ identCat $ normCat varCat)
                  field 
          in [ "final" +++ lhs +++ "=" +++ rhs ++ ";" ] 
              ++ traverseRule ind1 (ind2 + 1) restTs restVars lines
        Right _ -> traverseRule ind1 (ind2 + 1) restTs (var:restVars) lines
      where
        field = "ctx?.p_" ++ show ind1 ++ "_" ++ show ind2
        buildArgument :: Integer -> String -> String -> String
        buildArgument prec typeName name = 
          let precedence = case prec of 
            0 -> ""
            _ -> show prec
          in "build" ++ upperFirst typeName ++ precedence ++ "(" ++ name ++ ")"


    generateNullCheck :: [DartVar] -> [String]
    generateNullCheck [] = []
    generateNullCheck vars = 
      [ "if (" ]
      ++ (indent 1 [ intercalate " || " $ map condition vars ]) 
      ++ [ ") {" ]
      ++ (indent 1 [ "return null;" ])
      ++ [ "}" ]
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