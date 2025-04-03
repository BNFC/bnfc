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

import BNFC.Backend.Scala.Utils (safeTail, safeCatName, getSymbFromName, hasTokenCat, catToStrings, getFunName, inspectListRulesByCategory)
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options ( SharedOptions(lang, Options) )
import BNFC.Backend.Common.NamedVariables (firstLowerCase)

import Data.List (find, intercalate, isSuffixOf)
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import System.Directory.Internal.Prelude (fromMaybe)

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
    -- ++ inspectListRulesByCategory (ruleGroups cf)
  ] ++
  -- End the WorkflowParser object
  endWorkflowClass ++
  -- Add the WorkflowCompiler object
  addWorkflowCompiler

-- | Generate all parser rules from rule groups
generateAllRules :: [(Cat, [Rule])] -> [Doc]
generateAllRules catsAndRules =
  let
    -- Generate regular rules
    mainRules = concatMap generateRuleGroup catsAndRules
    
    -- Generate special rules for integer and string if needed
    integerRule = generateSpecialRule catInteger "integer" "INTEGER" "toInt" catsAndRules
    stringRule = generateSpecialRule catString "string" "STRING" "" catsAndRules
    
  in mainRules ++ integerRule ++ stringRule

-- | Generate a rule group (definition for a single category)
generateRuleGroup :: (Cat, [Rule]) -> [Doc]
generateRuleGroup (cat, rules) = 
  [text $ "def " ++ catName ++ ": Parser[WorkflowAST] = positioned {"] ++
  [nest 4 $ generateRuleBody rules] ++
  [text "}"]
  where
    catName = safeCatName cat

-- | Generate the body of a rule
generateRuleBody :: [Rule] -> Doc
generateRuleBody rules@(Rule _ cat _ _ : _) =
  if any (hasTokenCat catInteger) rules
  then text $ firstLowerCase catInteger
  else 
    let
      vars = concatMap prPrintRule_ rules
      exitCatName = firstLowerCase $ fromMaybe (error "Empty list encountered") $ listToMaybe $ filterNotEqual (show (wpThing cat)) $ filterSymbs vars
      
      headerText = exitCatName ++ " ~ rep((" ++ intercalate " | " (onlySymbs (safeTail vars)) ++ ") ~ " ++ exitCatName ++ ") ^^ {"
      subHeaderText = "case " ++ exitCatName ++ " ~ list => list.foldLeft(" ++ exitCatName ++ ") {"
      
      caseStatements = map generateCaseStatement rules
    in
      text headerText $+$
      nest 4 (text subHeaderText) $+$
      nest 8 (vcat $ map text $ filter (not . null) caseStatements) $+$
      nest 4 (text "}") $+$
      text "}"
generateRuleBody [] = empty



-- generateCaseStatement :: Rule -> String
-- generateCaseStatement r@(Rule fun _ _ _)
--   | isCoercion fun = ""
--   | null vars = fnm ++ "()"  -- Special case for an empty list
--   | otherwise = 
--       "case (" ++ head vars ++ concatMap (" ~ " ++) (safeTail vars) ++ ") => " 
--       ++ fnm ++ "(" ++ intercalate ", " (filterSymbs vars) ++ ")"
--   where
--     vars = disambiguateNames $ map modifyVars (prPrintRule_ r)
--     fnm = funName fun
--     modifyVars str
--       | "()" `isSuffixOf` str = str
--       | all isAlphaNum str = case str of
--                                (x:_) -> [toLower x]
--                                []    -> error "Empty string encountered in modifyVars"
--       | otherwise = "sym" ++ show (length str)  -- Replace invalid symbols with placeholders


-- -- | Generate a case statement for a rule
generateCaseStatement :: Rule -> String
generateCaseStatement r@(Rule fun _ _ _)
  | isCoercion fun = ""
  | null vars = fnm ++ "()"  -- Caso especial para lista vacía
  | otherwise = 
      "case (" ++ head vars ++ ", " ++ intercalate " ~ " (safeTail vars) ++ ") => "
      ++ fnm ++ "(" ++ intercalate ", " (filterSymbs vars) ++ ")"
  where
    vars = disambiguateNames $ map modifyVars (prPrintRule_ r)
    fnm = funName fun
    -- modifyVars str = if "()" `isSuffixOf` str 
    --                  then getSymbFromName str 
    --                  else case str of
    --                         (x:_) -> [toLower x]
    --                         []    -> error "Empty string encountered in modifyVars"
    modifyVars str
      | "()" `isSuffixOf` str = str
      | all isAlphaNum str = case str of
                               (x:_) -> [toLower x]
                               []    -> error "Empty string encountered in modifyVars"
      | otherwise = getSymbFromName str  -- Replace invalid symbols with placeho


-- | Generate a special rule for tokens like Integer or String
generateSpecialRule :: TokenCat -> String -> String -> String -> [(Cat, [Rule])] -> [Doc]
generateSpecialRule tokenCat ruleName tokenName conversion catsAndRules =
  case find (\(_, rules) -> any (hasTokenCat tokenCat) rules) catsAndRules of
    Just (_, rules) -> case find (hasTokenCat tokenCat) rules of
      Just rule -> 
        let 
          funName = getFunName rule
          conversionPart = if null conversion then "" else "." ++ conversion
        in
          [ text $ "def " ++ ruleName ++ ": Parser[" ++ funName ++ "] = positioned {"
          , nest 4 $ text $ "accept(\"" ++ ruleName ++ "\", { case " ++ tokenName ++ "(i) => " ++ funName ++ "(i" ++ conversionPart ++ ") })"
          , text "}"
          ]
      Nothing -> []
    Nothing -> []

-- | Extract terminal and non-terminal symbols from a rule
prPrintRule_ :: Rule -> [String]
prPrintRule_ (Rule _ _ items _) = map getSymbFromName $ catToStrings items


-- | Filter out strings ending with "()"
filterSymbs :: [String] -> [String]
filterSymbs = filter (not . isSuffixOf "()")

-- | Keep only strings ending with "()"
onlySymbs :: [String] -> [String]
onlySymbs = filter (isSuffixOf "()")

-- | Remove an element from a list
filterNotEqual :: Eq a => a -> [a] -> [a]
filterNotEqual element list = filter (/= element) list

-- | Make variable names unique by adding numbers to duplicates
disambiguateNames :: [String] -> [String]
disambiguateNames = disamb []
  where
    disamb ns1 (n:ns2)
      | n `elem` (ns1 ++ ns2) = let i = length (filter (==n) ns1) + 1
                               in (n ++ show i) : disamb (n:ns1) ns2
      | otherwise = n : disamb (n:ns1) ns2
    disamb _ [] = []

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
