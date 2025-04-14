{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Scala Parser syntax
    Copyright (Scala) 2024  Author:  Juan Pablo Poittevin, Guillermo Poladura

    Description   : This module generates the Scala Parser Syntax
                    Using Scala Parser Combinator

    Author        : Juan Pablo Poittevin, Guillermo Poladura
    Created       : 30 September, 2024
-}

module BNFC.Backend.Scala.CFtoScalaParser (cf2ScalaParser) where

import qualified Data.Foldable as DF (toList)
import Prelude hiding ((<>))

import BNFC.Backend.Scala.Utils (safeCatName, hasTokenCat, rhsToSafeStrings, disambiguateNames, getRHSCats, isSpecialCat, isListCat, isLeft, safeHeadChar, wildCardSymbs, scalaReserverWords, mapManualTypeMap, isCoercionCategory)
import BNFC.CF
    ( allEntryPoints,
      catChar,
      catIdent,
      catInteger,
      catString,
      isNilCons,
      normCat,
      ruleGroups,
      rulesForNormalizedCat,
      sameCat,
      sortRulesByPrecedence,
      CF,
      Cat,
      IsFun(isCoercion, funName),
      Rul(Rule, valRCat, rhsRule),
      Rule,
      TokenCat,
      WithPosition(wpThing), strToCat )
import BNFC.PrettyPrint
import BNFC.Options ( SharedOptions(lang, Options) )
import BNFC.Backend.Common.NamedVariables (firstLowerCase, fixCoercions)

import Data.List (find, intercalate, nub)
import Data.Char (toLower)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import BNFC.Utils ((+++), symbolToName)
import BNFC.Backend.Scala.CFtoScalaParserAST (getASTNames)
import GHC.Unicode (toUpper)
import BNFC.Backend.Scala.Utils (isSymbol)
import Debug.Trace (trace)

-- | Main function that generates the Scala parser code
cf2ScalaParser :: SharedOptions -> CF -> Doc
cf2ScalaParser Options{ lang } cf = vcat $
  -- Generate header and imports
  imports lang ++
  -- Start the WorkflowParser object
  initWorkflowClass ++
  -- Indent the class contents
  [nest 4 $ vcat $ 
    -- Add extra classes
    addExtraClasses ++
    [text ""] ++
    -- Add apply function
    getApplyFunction ++
    [text ""] ++
    -- Add program function
    getProgramFunction cf ++
    [text ""] ++
    -- Add parser rules
    generateAllRules cf (ruleGroups cf) ++
    [text ""] 
    -- inspect rules, only for debugging
    -- ++ inspectListRulesByCategory (ruleGroups cf)
  ] ++
  -- End the WorkflowParser object
  endWorkflowClass ++
  -- Add the WorkflowCompiler object
  addWorkflowCompiler

-- | Generate all parser rules from rule groups
generateAllRules :: CF -> [(Cat, [Rule])] -> [Doc]
generateAllRules cf catsAndRules =
  let
    -- filter categories with all the rules isNilCons
    rulesToProcess = filter (not . all isNilCons . snd) catsAndRules 
    -- Generate regular rules
    mainRules = map text $ concatMap (generateRuleGroup cf) (fixCoercions rulesToProcess)
    
    -- existe isUsedCat seguramente nos simplifique esto
    -- existe specialCats seguramente nos simplifique esto
    -- existe sigLookup wtf con esto
    -- Generate special rules for integer and string if needed
    integerRule = generateSpecialRule catInteger "integer" "INTEGER" "toInt" "pInteger" catsAndRules
    stringRule = generateSpecialRule catString "string" "STRING" "toString" "pString" catsAndRules
    charRule = generateSpecialRule catChar "char" "CHAR" "charAt(0)" "pChar" catsAndRules
    identRule = generateSpecialRule catIdent "ident" "IDENT" "toString" "pIdent" catsAndRules
    
  in mainRules ++ integerRule ++ stringRule ++ charRule ++ identRule


getRuleFunName :: Cat -> Rule -> String
getRuleFunName cat (Rule fnam _ _ _) = firstLowerCase $ prefixIfNeeded $ funName fnam
  where
    prefixIfNeeded name
      | map toLower name == map toLower (safeCatName cat) || isJust (scalaReserverWords (map toLower name))  = "internal_" ++ map toLower name
      | otherwise = name

getRulesFunsName :: [Rule] -> Cat -> [String]
getRulesFunsName rules cat = nub $ map (getRuleFunName cat) rules
  
-- | Generate a rule group (definition for a single category)
generateRuleGroup :: CF -> (Cat, [Rule]) -> [String]
generateRuleGroup cf (cat, rules) = 
  ["def " ++ catName ++ ": Parser[WorkflowAST] = positioned {"] ++
  [replicate 4 ' ' ++ intercalate " | " subFuns] ++
  ["}"] ++
  concatMap (generateSingleRuleBody cf cat) nonCoercionRules
  where
    subFuns = getRulesFunsName nonCoercionRules cat
    catName = safeCatName cat
    nonCoercionRules = reverse $ map snd $ sortRulesByPrecedence $ filter (not . isCoercion) rules

generateSingleRuleBody :: CF -> Cat -> Rule -> [String]
generateSingleRuleBody cf cat rule = [generateRuleDefinition cf cat rule ++ generateRuleTransformation rule]

generateRuleDefinition :: CF -> Cat -> Rule -> String
generateRuleDefinition cf cat rule =
  "def " ++ getRuleFunName cat rule ++ ": Parser[WorkflowAST] ="
    +++ intercalate " ~ " (generateRuleForm cf rule)


getBaseCatOfRecursiveRule :: Rule -> [String]
getBaseCatOfRecursiveRule rule@(Rule _ _ rhs _) =
  nub $ concatMap extractBaseCat rhs
  where
    -- Extrae las categorías base de un elemento del RHS
    extractBaseCat :: Either Cat String -> [String]
    extractBaseCat (Left cat)
      | isBaseCat cat = [getRuleFunName cat rule]
      | otherwise = []
    extractBaseCat (Right s) = if isSymbol s then [] else [getRuleFunName (strToCat s) rule]

    isBaseCat :: Cat -> Bool
    isBaseCat cat = isSpecialCat $ normCat cat


getBasesOfRecursiveRule :: CF -> Rule -> String
getBasesOfRecursiveRule cf rule =
  let
    allRulesForCat = rulesForNormalizedCat cf (normCat $ wpThing $ valRCat rule)
    baseTypes = nub $ concatMap getBaseCatOfRecursiveRule allRulesForCat
  in
    case baseTypes of
      [] -> ""
      x:[] -> x
      x:xs -> "(" ++ intercalate " | " (x:xs) ++ ")"


generateRuleForm :: CF -> Rule  -> [String]
generateRuleForm cf rule@(Rule _ _ rhs _) =
  if isRecursiveRule rule 
    then snd $ generateRecursiveRuleForm rhs False
    else case rhs of
      [Right s] -> [fromMaybe (paramS s) (mapManualTypeMap (paramS s)) ++ "()"]
      _ -> map (addRuleForListCat rhs) (rhsToSafeStrings rhs)
  where
    generateRecursiveRuleForm :: [Either Cat String] -> Bool -> (Bool, [String])
    generateRecursiveRuleForm [] added = (added, [])
    generateRecursiveRuleForm (r:rest) added =
      case r of
        Left cat ->
          if isCoercionCategory cat && not added
            then
              let (_, strsRest) = generateRecursiveRuleForm rest True
              in (True, getBasesOfRecursiveRule cf rule : strsRest)
            else
              let (addedRest, strsRest) = generateRecursiveRuleForm rest added
              in (addedRest, rhsToSafeStrings [r] ++ strsRest)
        Right _ ->
          let (addedRest, strsRest) = generateRecursiveRuleForm rest added
          in (addedRest, rhsToSafeStrings [r] ++ strsRest)

    paramS s = fromMaybe (map toUpper s) (symbolToName s)

generateRuleTransformation :: Rule -> String
generateRuleTransformation rule =
  if isRuleOnlySpecials rule
    then case rhsRule rule of
      [] -> "EMPTY() ^^ {" ++ generateCaseStatement rule ++ "}"
      _ -> ""
    else " ^^ { " ++ generateCaseStatement rule ++ " }"

isRecursiveRule :: Rule -> Bool
isRecursiveRule (Rule _ cat rhs _) =
  any (sameCat (wpThing cat)) (getRHSCats rhs)

addRuleForListCat :: [Either Cat String] -> String -> String
addRuleForListCat rhs s =
  case find (\cat -> isListCat cat && safeCatName cat == s) (getRHSCats rhs) of
    Just _ -> "rep(" ++ firstLowerCase s ++ ")"
    Nothing -> s

isRuleOnlySpecials :: Rule -> Bool
isRuleOnlySpecials (Rule _ _ rhs _) =
  all isSpecialCat (getRHSCats rhs) && all isLeft rhs

-- -- | Generate a case statement for a rule
generateCaseStatement :: Rule -> String
generateCaseStatement rule@(Rule fun c rhs _)
  | isCoercion fun = ""
  | null vars = "_ => " ++ fnm ++ "()"
  | otherwise = 
      "case (" ++ intercalate " ~ "  params ++ ") => "
      ++ fnm ++ "(" ++ intercalate ", " vars ++ ")"
  where
    getBaseType :: Cat -> String
    getBaseType cat
      | isListCat cat = "List[WorkflowAST]"
      | otherwise = "WorkflowAST"

    fnm = fromMaybe (getRuleFunName (wpThing c) rule) $ listToMaybe $ getASTNames [rule]

    -- generate a list of with (rule, finalName)
    zipped = zip rhs (disambiguateNames $ map getSymb rhs)

    getSymb (Left cat) = [toLower $ safeHeadChar $ safeCatName cat]
    getSymb (Right str) = [toLower $ safeHeadChar $ fromMaybe "_" $ symbolToName str]

    params = map (wildCardSymbs.snd) zipped

    -- For Left cat we assign types, for Right str (tokens), we ignore (or use "_")
    vars = [ p ++ ".asInstanceOf[" ++ getBaseType cat ++ "]"
           | (Left cat, p) <- zipped
           ]

-- | Generate a special rule for tokens like Integer or String
generateSpecialRule :: TokenCat -> String -> String -> String -> String -> [(Cat, [Rule])] -> [Doc]
generateSpecialRule tokenCat ruleName tokenName conversion pTypeName catsAndRules =
  case find (\(_, rules) -> any (hasTokenCat tokenCat) rules) catsAndRules of
    Just (_, rules) -> case find (hasTokenCat tokenCat) rules of
      Just _ -> 
        let 
          conversionPart = if null conversion then "" else "." ++ conversion
        in
          [ text $ "def " ++ ruleName ++ ": Parser[" ++ pTypeName ++ "] = {"
          , nest 4 $ text $ "accept(\"" ++ ruleName ++ "\", { case " ++ tokenName ++ "(i) => " ++ pTypeName ++ "(i" ++ conversionPart ++ ") })"
          , text "}"
          ]
      Nothing -> []
    Nothing -> []

-- | Generate the imports section
imports :: String -> [Doc]
imports name = [
  text $ "package " ++ name ++ ".workflowtoken." ++ name ++ "Parser",
  text "",
  text $ "import " ++ name ++ ".workflowtoken." ++ name ++ "Lex._",
  text "",
  text "import scala.util.parsing.combinator.Parsers",
  text "",
  text "import scala.util.parsing.input.{NoPosition, Position, Reader}"
  ]

-- | Generate the extra classes section
addExtraClasses :: [Doc]
addExtraClasses = [
  text "override type Elem = WorkflowToken",
  text "",
  text "class WorkflowTokenReader(tokens: Seq[WorkflowToken]) extends Reader[WorkflowToken] {",
  nest 4 $ text "override def first: WorkflowToken = tokens.head",
  nest 4 $ text "override def atEnd: Boolean = tokens.isEmpty",
  nest 4 $ text "override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)",
  nest 4 $ text "override def rest: Reader[WorkflowToken] = new WorkflowTokenReader(tokens.tail)",
  text "}"
  ]

-- | Generate the WorkflowCompiler object
addWorkflowCompiler :: [Doc]
addWorkflowCompiler = [
  text "",
  text "object WorkflowCompiler {",
  nest 4 $ text "def apply(code: String): Either[WorkflowCompilationError, WorkflowAST] = {",
  nest 8 $ text "for {",
  nest 12 $ text "tokens <- WorkflowLexer(code).right",
  nest 12 $ text "ast <- WorkflowParser(tokens).right",
  nest 8 $ text "} yield ast",
  nest 4 $ text "}",
  text "}"
  ]

-- | Start the WorkflowParser object
initWorkflowClass :: [Doc]
initWorkflowClass = [
  text "",
  text "object WorkflowParser extends Parsers {"
  ]

-- | End the WorkflowParser object
endWorkflowClass :: [Doc]
endWorkflowClass = [
  text "}"
  ]

-- | Generate the apply function
getApplyFunction :: [Doc]
getApplyFunction = [
  text "",
  text "def apply(tokens: Seq[WorkflowToken]): Either[WorkflowParserError, WorkflowAST] = {",
  nest 4 $ text "val reader = new WorkflowTokenReader(tokens)",
  nest 4 $ text "program(reader) match {",
  nest 8 $ text "case NoSuccess(msg, next) => Left(WorkflowParserError(Location(next.pos.line, next.pos.column), msg))",
  nest 8 $ text "case Success(result, next) => Right(result)",
  nest 4 $ text "}",
  text "}"
  ]

-- | Generate the program function
getProgramFunction :: CF -> [Doc]
getProgramFunction cf = [
  text "",
  text "def program: Parser[WorkflowAST] = positioned {",
  nest 4 $ text $ "phrase(" ++ fromMaybe entryPoint (scalaReserverWords entryPoint) ++ ")",
  text "}"
  ]
  where
    entryPoint = case listToMaybe (map normCat $ DF.toList $ allEntryPoints cf) of
      Just ep -> firstLowerCase $ show ep
      Nothing -> error "No entry points found in the context-free grammar."
