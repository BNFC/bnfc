{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Scala Parser syntax
    Copyright (Scala) 2024  Author:  Juan Pablo Poittevin

    Description   : This module generates the Scala Parser Syntax
                    Using Scala Parser Combinator
    Author        : Juan Pablo Poittevin
    Created       : 30 September, 2024
-}

module BNFC.Backend.Scala.CFtoScalaParser (cf2ScalaParser) where

import Prelude hiding ((<>))

import qualified Data.Foldable as DF (toList)
import BNFC.Utils (symbolToName)
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options
import Data.List (find, intercalate, isSuffixOf )
import BNFC.Backend.Common.NamedVariables (firstLowerCase)
import Data.Char (toLower)

cf2ScalaParser
  :: SharedOptions     
  -> CF     -- Grammar.
  -> Doc 
cf2ScalaParser Options{ lang } cf = vsep . concat $
  [ 
      []
    , imports lang
    , initWorkflowClass
    , map (nest 4) addExtraClasses
    , map (nest 4) getApplyFunction
    , map (nest 4) (getProgramFunction cf)
    , map (nest 4) strRules
    , endWorkflowClass
    , addWorkflowCompiler
  ]
  where
    strRules     = ruleGroupsCFToString (ruleGroups cf)

catToStrings :: [Either Cat String] -> [String]
catToStrings = map (\case
                  Left c -> show c
                  Right s -> s
                )

getSymbFromName :: String -> String
getSymbFromName s = 
  case symbolToName s of
    Just s -> s ++ "()"
    _ -> s

prPrintRule_ :: Rule -> [String]
prPrintRule_ (Rule _ _ items _) = map getSymbFromName $ catToStrings items

-- Function to extract the function name from a rule
getFunName :: Rule -> String
getFunName (Rule fun _ _ _) = (wpThing fun)

-- Helper to check if a rule contains a TokenCat in RHS
hasTokenCat :: TokenCat -> Rule -> Bool
hasTokenCat token (Rule _ _ rhs _) = TokenCat token `elem` [c | Left c <- rhs]

safeTail :: [a] -> [a]
safeTail []     = []  -- Si la lista está vacía, devuelve una lista vacía
safeTail (_:xs) = xs 

filterSymbs :: [[Char]] -> [[Char]]
filterSymbs = filter (not . isSuffixOf "()")

onlySymbs :: [[Char]] -> [[Char]]
onlySymbs = filter (isSuffixOf "()")

prSubRule :: Rule -> [String]
prSubRule r@(Rule fun _ _ _) 
  | isCoercion fun = []
  | otherwise = 
      ["case (" ++ head vars ++ ", " ++ intercalate " ~ " (safeTail vars) ++ ") => " 
      ++ fnm ++ "(" ++ intercalate ", " (filterSymbs vars) ++ ")"]
  where
    vars = disambiguateNames $ map modifyVars (prPrintRule_ r)
    fnm = funName fun
    modifyVars str = if "()" `isSuffixOf` str then str else [toLower (head str)]


coerCatDefSign :: Cat -> Doc
coerCatDefSign cat = 
      text $ "def " ++ pre ++ ": Parser[WorkflowAST] = positioned {"
  where
    pre = firstLowerCase $ show cat

prSubRuleDoc :: Rule -> [Doc]
prSubRuleDoc r = map text (prSubRule r)

filterNotEqual :: Eq a => a -> [a] -> [a]
filterNotEqual element list = filter (/= element) list

rulesToString :: [Rule] -> [Doc]
rulesToString [] = [""]
rulesToString rules@(Rule _ cat _ _ : _) =
    let 
        -- Filtramos las reglas que pertenecen a la misma categoría `cat`
        (sameCat, rest) = span (\(Rule _ c _ _) -> c == cat) rules
        
        -- Obtenemos los símbolos de las reglas filtradas
        vars = concatMap prPrintRule_ sameCat
        exitCatName = firstLowerCase $ head $ filterNotEqual (show (wpThing cat)) $ filterSymbs vars
        
        integerRuleData = case sameCat of 
                            (fRule:_) -> hasTokenCat catInteger fRule
                            [] -> False

        -- Si no es "integer", generamos la cabecera única con las alternativas en `rep(...)`
        header = [text $ exitCatName ++ " ~ rep((" ++ intercalate " | " (onlySymbs (safeTail vars)) ++ ") ~ " ++ exitCatName ++ ") ^^ {"]
        subHeader = [nest 4 $ text $ "case " ++ exitCatName ++ " ~ list => list.foldLeft(" ++ exitCatName ++ ") {"]

        -- Generamos los `case` correspondientes a las reglas de `sameCat`
        rulesDocs = map (nest 8) $ concatMap prSubRuleDoc sameCat
        
        -- Si integerRuleData es True, evitamos agregar header y subHeader
        mainBlock = if integerRuleData 
                      then [text $ firstLowerCase catInteger ]  -- TODO: change this to a map between cat and function name example (CatInteger -> integer)
                      else header ++ subHeader ++ rulesDocs ++ [nest 4 "}", "}"]

    in mainBlock  ++ rulesToString rest
  

ruleGroupsCFToString :: [(Cat, [Rule])] -> [Doc]
ruleGroupsCFToString catsAndRules = 
  let
      -- Process each group to generate its rules
      processGroup (c, r) = [coerCatDefSign c] ++ map (nest 4) (rulesToString r) ++ ["}"]

      -- Find the first rule where `TokenCat catInteger` appears
      integerRuleData = find (\(_, rules) -> any (hasTokenCat catInteger) rules) catsAndRules
      stringRuleData  = find (\(_, rules) -> any (hasTokenCat catString) rules) catsAndRules

      -- Generate the integer rule only if needed
      integerRule = case integerRuleData of
                      Just (_, rules) -> case find (hasTokenCat catInteger) rules of
                        Just rule -> 
                          let funName = getFunName rule in
                          [ text $ "def integer: Parser[" ++ funName ++ "] = positioned {"
                          , nest 4 $ text $ "accept(\"integer\", { case INTEGER(i) => " ++ funName ++ "(i.toInt) })"
                          , "}"
                          ]
                        Nothing -> []
                      Nothing -> []

      -- Generate the string rule only if needed
      stringRule = case stringRuleData of
                      Just (_, rules) -> case find (hasTokenCat catString) rules of
                        Just rule -> 
                          let funName = getFunName rule in
                          [ text $ "def string: Parser[" ++ funName ++ "] = positioned {"
                          , nest 4 $ text $ "accept(\"string\", { case STRING(s) => " ++ funName ++ "(s) })"
                          , "}"
                          ]
                        Nothing -> []
                      Nothing -> []

      -- Generate all main rules
      mainGeneratedRules = concatMap processGroup catsAndRules

  in mainGeneratedRules ++ integerRule ++ stringRule


imports :: String -> [Doc]
imports name  = [
   text $ "package " ++ name ++ ".workflowtoken." ++ name ++ "Parser"
   , text $ "import " ++ name ++ ".workflowtoken." ++ name ++ "Lex._"
   , "import scala.util.parsing.combinator.Parsers"
   , "import scala.util.parsing.input.{NoPosition, Position, Reader}"
  ]


addExtraClasses :: [Doc]
addExtraClasses = [
    "class WorkflowTokenReader(tokens: Seq[WorkflowToken]) extends Reader[WorkflowToken] {"
    , nest 4 "override def first: WorkflowToken = tokens.head"
    , nest 4  "override def atEnd: Boolean = tokens.isEmpty"
    , nest 4  "override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)"
    , nest 4  "override def rest: Reader[WorkflowToken] = new WorkflowTokenReader(tokens.tail)"
    , "}"
  ]

addWorkflowCompiler :: [Doc]
addWorkflowCompiler = [
    "object WorkflowCompiler {"
    , nest 4 "def apply(code: String): Either[WorkflowCompilationError, WorkflowAST] = {"
    , nest 8  "for {"
    , nest 12  "tokens <- WorkflowLexer(code).right"
    , nest 12  "ast <- WorkflowParser(tokens).right"
    , nest 8 "} yield ast"
    , nest 4 "}"
    , "}"
  ]

initWorkflowClass :: [Doc]
initWorkflowClass = [
      "object WorkflowParser extends Parsers {"
    , nest 4 "override type Elem = WorkflowToken"
    -- TODO: I REMOVED THIS LINE TO MAKE IT WORK, BUT WILL FAIL WITH GRAMMARS WHICH USE TABS/SPACES AS BLOCKS DEFINITION, SHOULD WE WORK ON THIS?
    -- , nest 4 "override def skipWhitespace = true"
    -- In case the lenguage use indentation for block creation, we should remove \n from the list of whiteSpace.
    -- , nest 4 $ text $ "override val whiteSpace = " ++ "\"" ++ "[\\t\\r\\f\\n]+\".r"
  ]

endWorkflowClass :: [Doc]
endWorkflowClass = [
    "}"
  ]

getApplyFunction :: [Doc]
getApplyFunction = [
     "def apply(tokens: Seq[WorkflowToken]): Either[WorkflowParserError, WorkflowAST] = {"
    , nest 4  "val reader = new WorkflowTokenReader(tokens)"
    , nest 4  "program(reader) match {"
    , nest 4    "case NoSuccess(msg, next) => Left(WorkflowParserError(Location(next.pos.line, next.pos.column), msg))"
    , nest 4    "case Success(result, next) => Right(result)"
    , nest 4  "}"
    ,  "}"
  ]



getProgramFunction :: CF -> [Doc]
getProgramFunction cf = [
    "def program: Parser[WorkflowAST] = positioned {"
    , nest 4 $ text $ "phrase(" ++ eps ++ ")"
    ,"}"
  ]
  where
    eps = firstLowerCase $ show $ head $ map normCat $ DF.toList $ allEntryPoints cf


disambiguateNames :: [String] -> [String]
disambiguateNames = disamb []
  where
    disamb ns1 (n:ns2)
      | n `elem` (ns1 ++ ns2) = let i = length (filter (==n) ns1) + 1
                                in (n ++ show i) : disamb (n:ns1) ns2
      | otherwise = n : disamb (n:ns1) ns2
    disamb _ [] = []
