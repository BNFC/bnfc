
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

cf2ScalaParserAST
  :: SharedOptions     
  -> CF
  -> Doc
cf2ScalaParserAST Options{ lang } cf = vsep . concat $
  [ 
    headers lang
    , [text $ concat $ map generateSymbClass symbs]
    , [generateStringClasses liters]
  ]
  where
    liters = literals cf
    symbs = unicodeAndSymbols cf


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

headers :: String -> [Doc]
headers name = [
  text $ "package " ++ name ++ ".WorkflowAST." ++ name ++ "ParserAST"
  , "import scala.util.parsing.input.Positional"
  , "sealed trait WorkflowAST extends Positional"
  ]


