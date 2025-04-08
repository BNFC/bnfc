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
    ( ruleGroups,
      CF,
      Cat(ListCat),
      IsFun(funName, isCoercion),
      Rul(Rule),
      Rule,
      WithPosition(wpThing), TokenCat, literals )
import BNFC.PrettyPrint ( text, vcat, Doc )
import BNFC.Options ( SharedOptions(lang, Options) )
import BNFC.Backend.Scala.Utils (generateVarsList, isLeft, baseTypeToScalaType, wrapList)
import Data.List (intercalate)
import BNFC.Utils ((+++))
import Data.Maybe (fromMaybe)

-- | Main function that generates the AST code
cf2ScalaParserAST :: SharedOptions -> CF -> Doc
cf2ScalaParserAST Options{ lang } cf = vcat $
  -- Generate headers
  headers lang ++
  -- Add an empty line after the trait definition
  [text ""] ++
  -- Generate case class definitions
  generateRuleDefs rules ++
  generateLiteralsDefs allLiterals
  where
    rules = ruleGroups cf
    allLiterals = literals cf

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

-- | Generate the class params
generateClassParams :: Rule -> String
generateClassParams (Rule _ _ rhs _) = 
  intercalate ", " $ zipWith (\x y -> x ++ ":" +++ y) (generateVarsList filteredRhs) (map catParams filteredRhs)
  where
    -- Function to format parameters based on whether they are Cat or String
    catParams :: Either Cat String -> String
    catParams (Left c)  = formatParamType c
    catParams (Right _) = "WorkflowAST"

    filteredRhs = filter isLeft rhs


-- | Format a parameter with its type
formatParamType :: Cat -> String
formatParamType cat = wrapList cat "WorkflowAST"  -- Si no es baseTokenCat, usar WorkflowAST

-- | Create a single case class definition
createCaseClass :: Rule -> Doc
createCaseClass rule@(Rule fun cat _ _)
  | ListCat _ <- (wpThing cat) = "" -- TODO: here we should process the list
  | otherwise = text $ formatCaseClass className params
  where
    className = funName fun
    -- Apply `catParams` to each element of `rhs`
    params = generateClassParams rule

-- | Helper function to format the case class definition
formatCaseClass :: String -> String -> String
formatCaseClass className params = "case class " ++ className ++ "(" ++ params ++ ") extends WorkflowAST"

-- | Generate the Scala types for basic LBNF types
generateLiteralsDefs :: [TokenCat] -> [Doc]
generateLiteralsDefs tokens = map (\token -> text $ formatCaseClass ("p" ++ token) ("var1: " ++ fromMaybe token (baseTypeToScalaType token))) tokens

-- | Generate the header part of the file
headers :: String -> [Doc]
headers name = [
  text $ "package " ++ name ++ ".workflowtoken." ++ name ++ "Parser",
  text "",
  text "import scala.util.parsing.input.Positional",
  text "",
  text "sealed trait WorkflowAST extends Positional"
  ]