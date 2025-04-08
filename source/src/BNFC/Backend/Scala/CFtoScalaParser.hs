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
import GHC.Unicode (isAlphaNum)

import BNFC.Backend.Scala.Utils (safeCatName, getSymbFromName, hasTokenCat, rhsToSafeStrings, disambiguateNames, getRHSCats, isSpecialCat, safeCatToStrings, inspectListRulesByCategory, isListCat, isLeft, safeHeadChar, disambiguateTuples, baseTypeToScalaType)
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options ( SharedOptions(lang, Options) )
import BNFC.Backend.Common.NamedVariables (firstLowerCase, fixCoercions)

import Data.List (find, intercalate, isSuffixOf, nub)
import Data.Char (toLower, toUpper)
import Data.Maybe (listToMaybe)
import BNFC.Utils ((+++))

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
    -- Add apply function
    getApplyFunction ++
    -- Add program function
    getProgramFunction cf ++
    -- Add parser rules
    generateAllRules (ruleGroups cf)
    -- inspect rules, only for debugging
    ++ inspectListRulesByCategory (ruleGroups cf)
  ] ++
  -- End the WorkflowParser object
  endWorkflowClass ++
  -- Add the WorkflowCompiler object
  addWorkflowCompiler

-- | Generate all parser rules from rule groups
generateAllRules :: [(Cat, [Rule])] -> [Doc]
generateAllRules catsAndRules =
  let
    -- filter categories with all the rules isNilCons
    -- TODO: check if this is correct, I'm assuming all the lists are processed using the rep and the scala List
    rulesToProcess = filter (not . all isNilCons . snd) catsAndRules 
    -- Generate regular rules
    mainRules = map text $ concatMap generateRuleGroup (fixCoercions rulesToProcess)
    
    -- Generate special rules for integer and string if needed
    integerRule = generateSpecialRule catInteger "integer" "INTEGER" "toInt" "pInteger" catsAndRules
    stringRule = generateSpecialRule catString "string" "STRING" "toString" "pString" catsAndRules
    charRule = generateSpecialRule catChar "char" "CHAR" "charAt(0)" "pChar" catsAndRules
    identRule = generateSpecialRule catIdent "ident" "IDENT" "toString" "pString" catsAndRules
    
  in mainRules ++ integerRule ++ stringRule ++ charRule ++ identRule

getRuleFunName :: Rule -> String
getRuleFunName (Rule fnam _ _ _) = firstLowerCase $ funName fnam

getRulesFunsName :: [Rule] -> [String]
getRulesFunsName rules = nub $ map getRuleFunName rules
  
-- | Generate a rule group (definition for a single category)
generateRuleGroup :: (Cat, [Rule]) -> [String]
generateRuleGroup (cat, rules) = 
  ["def " ++ catName ++ ": Parser[WorkflowAST] = positioned {"] ++
  [replicate 4 ' ' ++ intercalate " | " (getRulesFunsName nonCoercionRules)] ++
  ["}"] ++
  concatMap generateRuleFor nonCoercionRules
  where
    catName = safeCatName cat
    nonCoercionRules = filter (not . isCoercion) rules
    -- rulesToProcess = filter (\rule -> not (isListCat (wpThing $ valRCat rule))) nonCoercionRules

    generateRuleFor :: Rule -> [String]
    generateRuleFor rule = generateRuleBody [rule]

-- | Based on a list of rules, generate the functions for the parser
generateRuleBody :: [Rule] -> [String]
generateRuleBody rules = concatMap generateSingleRuleBody rules

generateSingleRuleBody :: Rule -> [String]
generateSingleRuleBody rule@(Rule _ _ _ _) = [generateRuleDefinition rule ++ generateRuleTransformation rule]

generateRuleDefinition :: Rule -> String
generateRuleDefinition rule =
  "def " ++ getRuleFunName rule ++ ": Parser[WorkflowAST] ="
    +++ intercalate " ~ " (generateRuleForm rule)

generateRuleForm :: Rule -> [String]
generateRuleForm rule@(Rule _ _ rhs _) =
  if isRecursiveRule rule
    then case rhsToSafeStrings rhs of
      (_ : rest) -> "integer" : rest
      [] -> [""] -- Handle empty rhs case
    else case rhs of
      [Right s] -> [map toUpper s ++ "()"]
      _ -> map (addRuleForListCat rhs) (rhsToSafeStrings rhs)

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
generateCaseStatement rule@(Rule fun _ _ _)
  | isCoercion fun = ""
  | null vars = "_ => " ++ fnm ++ "()"
  | otherwise = 
      "case (" ++ intercalate " ~ "  params ++ ") => "
      ++ fnm ++ "(" ++ intercalate ", " vars ++ ")"
  where
    getRHSParamsFromRule :: Rule -> [String]
    getRHSParamsFromRule (Rule _ _ items _) = 
      disambiguateNames $ map modifyParams $ map getSymbFromName $ safeCatToStrings items

    getFunVarsWithTypeFromRule :: Rule -> [(String, String)]
    getFunVarsWithTypeFromRule (Rule _ _ items _) = 
      disambiguateTuples $ map (\(c, s) -> (modifyVars (getSymbFromName (safeCatName c)), s)) $ map addTypesToVars $ getRHSCats items

    getBaseType :: Cat -> String
    getBaseType cat
      | isListCat cat = "List[WorkflowAST]"
      | otherwise = "WorkflowAST"

    addTypesToVars :: Cat -> (Cat, String)
    addTypesToVars cat = (cat, ".asInstanceOf[" ++ getBaseType cat ++ "]")

    params = concatMap getRHSParamsFromRule [rule]
    vars = map (\(var, typ) -> var ++ typ) $ getFunVarsWithTypeFromRule rule
    fnm = funName fun

    modifyVars str 
      | all isAlphaNum str = [toLower $ safeHeadChar str]
      | otherwise = [toLower $ safeHeadChar $ getSymbFromName str] 

    modifyParams str 
      | "()" `isSuffixOf` str = "_"
      | all isAlphaNum str = [toLower $ safeHeadChar str]
      | otherwise = [toLower $ safeHeadChar $ getSymbFromName str] 


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
  nest 4 $ text $ "phrase(" ++ entryPoint ++ ")",
  text "}"
  ]
  where
    entryPoint = case listToMaybe (map normCat $ DF.toList $ allEntryPoints cf) of
      Just ep -> firstLowerCase $ show ep
      Nothing -> error "No entry points found in the context-free grammar."
