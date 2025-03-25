
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
import BNFC.Utils (symbolToName, camelCase)
import Data.Char (toUpper)

import BNFC.Backend.Common.NamedVariables (firstLowerCase)
import Data.List (intercalate)
import Data.Map (Map, fromList, lookup)

cf2ScalaParserAST
  :: SharedOptions     
  -> CF
  -> Doc
cf2ScalaParserAST Options{ lang } cf = vsep . concat $
  [ 
    headers lang
    , strRules
  ]
  where
    strRules     = prRulesNames rules
    rules        = ruleGroups cf


-- case class EAdd(exp: WorkflowAST, exp1: WorkflowAST) extends WorkflowAST

catFilterToStrings :: [Either Cat String] -> [String]
catFilterToStrings = map (\case
                  Left c -> show c
                  Right _ -> ""
                )


prRuleName :: Rule -> [Doc]
prRuleName (Rule fun _ rhs _) 
  | isCoercion fun = [""]
  | otherwise = [
      text $ "case class " ++ fnm ++ "("
    , text $ intercalate ", " $ map formatParam parsNames
    , " ) extends WorkflowAST"
    ]
  where
    fnm = funName fun
    parsNames = filter (not . null) $ map firstLowerCase (catFilterToStrings rhs)

    -- Si el parámetro es "integer", lo tipamos como Int; si no, como WorkflowAST
    formatParam param
      | (show (camelCase param)) `elem` baseTokenCatNames = param ++ ": " ++  case baseTypeToScalaType param of -- convertir a camelCase el param, seguramente no sea le mejor forma de detemrinar el tipo, otra?
                                                Just s -> s
                                                _ -> "Int"  -- TODO: Int no deberia ser el default, deberia fallar, creo
      | otherwise          = param ++ ": WorkflowAST"


-- prRuleName :: Rule -> [Doc]
-- prRuleName r@(Rule fun _ rhs _) 
--   -- | isCoercion fun = prCoerciveRule r
--   | isCoercion fun = [""]
--   | otherwise = [
--       text $ "case class " ++ fnm ++ "("
--     , text $ intercalate ", " $ map (++ ": WorkflowAST") parsNames
--     -- , intersperse (text ", ") (filter (not . null) [text (param ++ ": WorkflowAST") | param <- parsNames, not (null param)])
--     , " ) extends WorkflowAST"
--     ]
--   where
--     fnm = funName fun
--     parsNames = filter (not . null) $ map firstLowerCase (catFilterToStrings rhs)

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


baseTypeToScalaType :: String -> Maybe String
baseTypeToScalaType = (`Data.Map.lookup` baseTypeMap)

-- | Map from base LBNF Type to scala Type.

baseTypeMap :: Map String String
baseTypeMap = fromList scalaTypesMap

scalaTypesMap :: [(String, String)]
scalaTypesMap =
  [ ("Integer"  , "Int")
  , ("String"   , "String")
  , ("Double"   , "Double")
  ]