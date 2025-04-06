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
      IsFun(isCoercion),
      Rul(Rule),
      Rule,
      WithPosition(wpThing) )
import BNFC.PrettyPrint ( text, vcat, Doc )
import BNFC.Options ( SharedOptions(lang, Options) )
import BNFC.Backend.Scala.Utils (generateClassSignature)

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
createCaseClass rule@(Rule _ cat _ _)
  | ListCat _ <- (wpThing cat) = "" -- TODO: here we should process the list
  | otherwise        = text $ "case class " ++ generateClassSignature rule True ++ " extends WorkflowAST"


-- | Generate the header part of the file
headers :: String -> [Doc]
headers name = [
  text $ "package " ++ name ++ ".workflowtoken." ++ name ++ "Parser",
  text "",
  text "import scala.util.parsing.input.Positional",
  text "",
  text "sealed trait WorkflowAST extends Positional"
  ]
