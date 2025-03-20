
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Scala Lextract syntax
    Copyright (Scala) 2024  Author:  Juan Pablo Poittevin

    Description   : This module generates the Scala Lextract Syntax
                    tree classes. It generates both a Header file
                    and an Implementation file

    Author        : Juan Pablo Poittevin
    Created       : 30 September, 2024
-}

module BNFC.Backend.Scala.CFtoScalaParserAST (cf2ScalaParserAST) where

import Prelude hiding ((<>))

import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options
import BNFC.Backend.Common (unicodeAndSymbols)
import BNFC.Utils (symbolToName)
import Data.Char (toUpper)

import BNFC.Backend.Common.NamedVariables (firstLowerCase)
import Data.List (intercalate)

cf2ScalaParserAST
  :: SharedOptions     
  -> CF
  -> Doc
cf2ScalaParserAST Options{ lang } cf = vsep . concat $
  [ 
    headers lang
    -- , [text $ concat $ map generateSymbClass symbs]
    -- , [generateStringClasses liters]
    -- , map (text.show) parserCats
    , strRules
  ]
  where
    liters       = literals cf
    symbs        = unicodeAndSymbols cf
    parserCats   = allParserCats cf
    strRules     = prRulesNames rules
    rules        = ruleGroups cf
    


generateSymbClass :: String -> String
generateSymbClass symb = case symbolToName symb of 
  Just s -> "case class " ++ s ++ "() extends WorkflowAST \n"
  Nothing -> ""


generateIntegerClasses :: [String] -> Doc
generateIntegerClasses params = text $ concat $ map generateIntegerClass params

generateIntegerClass :: String -> String
generateIntegerClass param = "case class " ++ param ++ "(i: Int) extends WorkflowAST \n"

generateStringClasses :: [String] -> Doc
generateStringClasses params = text $ concat $ map generateStringClass params

generateStringClass :: String -> String
generateStringClass param = "case class " ++ (map toUpper param) ++ "(str: String) extends WorkflowAST \n"


-- case class EAdd(exp: WorkflowAST, exp1: WorkflowAST) extends WorkflowAST

catFilterToStrings :: [Either Cat String] -> [String]
catFilterToStrings = map (\case
                  Left c -> show c
                  Right s -> ""
                )


prRuleName :: Rule -> [Doc]
prRuleName r@(Rule fun cat rhs _) 
  -- | isCoercion fun = prCoerciveRule r
  | isCoercion fun = [""]
  | otherwise = [
      text $ "case class " ++ fnm ++ "("
    , text $ intercalate ", " $ map (++ ": WorkflowAST") parsNames
    -- , intersperse (text ", ") (filter (not . null) [text (param ++ ": WorkflowAST") | param <- parsNames, not (null param)])
    , " ) extends WorkflowAST"
    ]
  where
    fnm = funName fun
    parsNames = filter (not . null) $ map firstLowerCase (catFilterToStrings rhs)

ruleIterPr :: [Rule] -> [Doc]
ruleIterPr  []     = [""]
ruleIterPr (r:[])  = prRuleName r
ruleIterPr (r:rs)  = prRuleName r ++ ruleIterPr rs


prRulesNames :: [(Cat,[Rule])] -> [Doc]
prRulesNames ( [] )         = [""]
prRulesNames ((_, r):[] )   = ruleIterPr r
prRulesNames ((_, r):crs )  = ruleIterPr r ++ prRulesNames crs


headers :: String -> [Doc]
headers name = [
  text $ "package " ++ name ++ ".workflowtoken." ++ name ++ "Parser"
  , "import scala.util.parsing.input.Positional"
  , "sealed trait WorkflowAST extends Positional"
  ]


