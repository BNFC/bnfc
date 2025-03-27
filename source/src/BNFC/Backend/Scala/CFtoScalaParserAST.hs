{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Scala Lextract syntax
    Copyright (Scala) 2024  Author:  Juan Pablo Poittevin, Guillermo Poladura

    Description   : This module generates the Scala Lextract Syntax
                    tree classes. It generates both a Header file
                    and an Implementation file

    Author        : Juan Pablo Poittevin, Guillermo Poladura
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

-- | Main function that generates the AST code
cf2ScalaParserAST :: SharedOptions -> CF -> Doc
cf2ScalaParserAST Options{ lang } cf = vcat $
  -- Generate headers
  headers lang ++
  -- Add an empty line after the trait definition
  [text ""] ++
  -- Generate case class definitions
  generateRuleDefs rules
  where
    rules = ruleGroups cf

-- | Generate all case class definitions
generateRuleDefs :: [(Cat, [Rule])] -> [Doc]
generateRuleDefs [] = []
generateRuleDefs rules = concatMap processRuleGroup rules

-- | Process a single rule group and generate case classes
processRuleGroup :: (Cat, [Rule]) -> [Doc]
processRuleGroup (_, rules) = map createCaseClass (filter (not . isCoercionRule) rules)

-- | Check if a rule is a coercion rule
isCoercionRule :: Rule -> Bool
isCoercionRule (Rule fun _ _ _) = isCoercion fun

-- | Create a single case class definition
createCaseClass :: Rule -> Doc
createCaseClass (Rule fun _ rhs _) = 
  text $ "case class " ++ className ++ "(" ++ params ++ ") extends WorkflowAST"
  where
    className = funName fun
    paramNames = filter (not . null) $ map firstLowerCase (catFilterToStrings rhs)
    params = intercalate ", " $ map formatParam paramNames

-- | Format a parameter with its type
formatParam :: String -> String
formatParam param
  | (show (camelCase param)) `elem` BNFC.CF.baseTokenCatNames = 
      param ++ ": " ++ case baseTypeToScalaType param of
                         Just s -> s
                         _ -> "Int"  -- Default to Int
  | otherwise = param ++ ": WorkflowAST"

-- | Extract category strings from rule RHS
catFilterToStrings :: [Either Cat String] -> [String]
catFilterToStrings = map (\case
                  Left c -> show c
                  Right _ -> ""
                )

-- | Generate the header part of the file
headers :: String -> [Doc]
headers name = [
  text $ "package " ++ name ++ ".workflowtoken." ++ name ++ "Parser",
  text "",
  text "import scala.util.parsing.input.Positional",
  text "",
  text "sealed trait WorkflowAST extends Positional"
  ]

-- | Convert base LBNF type to Scala type
baseTypeToScalaType :: String -> Maybe String
baseTypeToScalaType = (`Data.Map.lookup` baseTypeMap)

-- | Map from base LBNF Type to scala Type
baseTypeMap :: Map String String
baseTypeMap = fromList scalaTypesMap

-- | Scala types mapping
scalaTypesMap :: [(String, String)]
scalaTypesMap =
  [ ("Integer"  , "Int")
  , ("String"   , "String")
  , ("Double"   , "Double")
  ]