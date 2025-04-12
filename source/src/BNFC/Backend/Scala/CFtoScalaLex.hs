{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Scala Lextract syntax
    Copyright (Scala) 2024  Author:  Juan Pablo Poittevin, Guillermo Poladura

    Description   : This module generates the Scala Lextract Syntax
                    tree classes. Using Scala Parser Combinator
                    
    Author        : Juan Pablo Poittevin, Guillermo Poladura
    Created       : 30 September, 2024
-}

module BNFC.Backend.Scala.CFtoScalaLex (cf2ScalaLex) where

import Prelude hiding ((<>))

import BNFC.Utils (symbolToName)
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options
import BNFC.Backend.Common (unicodeAndSymbols)
import Data.List (intercalate)
import Data.Char (toLower)
import GHC.OldList (nub)
import GHC.Unicode (toUpper)

-- | Converts a string to lowercase
toLowerString :: String -> String
toLowerString s = map toLower s

-- | Main function that generates the Scala lexer code
cf2ScalaLex :: SharedOptions -> CF -> Doc
cf2ScalaLex Options{ lang } cf = vcat $
  -- Generate header and imports
  imports lang ++
  -- Generate error and location classes
  addExtraClasses ++
  -- Start the WorkflowLexer object
  initWorkflowClass ++
  -- Indent the class contents
  [nest 4 $ vcat $ 
    -- Add apply function
    getApplyFunction ++
    -- Add tokens function
    getTokensFunction (symbs ++ keyWords ++ liters) ++
    -- Add parser functions for literals
    map addParserFunction liters ++
    -- Add keyword parsers
    map getBaseLexs (keyWords ++ symbs)
  ] ++
  -- End the WorkflowLexer object
  endWorkflowClass
  where
    keyWords = reservedWords cf
    symbs  = unicodeAndSymbols cf
    liters = nub $ literals cf

-- | Generate a parser function for a specific literal type
addParserFunction :: String -> Doc
addParserFunction liter 
  | liter == catInteger = getIntegerFunction
  | liter == catDouble  = getDoubleFunction
  | liter == catIdent   = getIdentifiersFunction
  | liter == catString  = getLiteralsFunction
  | liter == catChar    = getLiteralFunction
  | otherwise = empty

-- | Generate the imports section
imports :: String -> [Doc]
imports name = [
  text $ "package " ++ name ++ ".workflowtoken." ++ name ++ "Lex",
  text "",
  text "import scala.util.parsing.combinator.RegexParsers"
  ]

-- | Generate error and location classes
addExtraClasses :: [Doc]
addExtraClasses = [
  text "",
  text "sealed trait WorkflowCompilationError",
  text "case class WorkflowLexerError(location: Location, msg: String) extends WorkflowCompilationError",
  text "case class WorkflowParserError(location: Location, msg: String) extends WorkflowCompilationError",
  text "",
  text "case class Location(line: Int, column: Int) {",
  nest 4 $ text "override def toString = s\"$line:$column\"",
  text "}"
  ]

-- | Start the WorkflowLexer object
initWorkflowClass :: [Doc]
initWorkflowClass = [
  text "",
  text "object WorkflowLexer extends RegexParsers {"
  ]

-- | End the WorkflowLexer object
endWorkflowClass :: [Doc]
endWorkflowClass = [
  text "}"
  ]

-- | Generate the apply function
getApplyFunction :: [Doc]
getApplyFunction = [
  text "def apply(code: String): Either[WorkflowLexerError, List[WorkflowToken]] = {",
  nest 4 $ text "parse(tokens, code) match {",
  nest 8 $ text "case NoSuccess(msg, next) => Left(WorkflowLexerError(Location(next.pos.line, next.pos.column), msg))",
  nest 8 $ text "case Success(result, next) => Right(result)",
  nest 4 $ text "}",
  text "}"
  ]

-- | Generate the function for parsing identifiers
getIdentifiersFunction :: Doc
getIdentifiersFunction = vcat [
  text "",
  text "def ident: Parser[IDENT] = {",
  nest 4 $ text "\"[a-zA-Z_][a-zA-Z0-9_]*\".r ^^ { str => IDENT(str) }",
  text "}"
  ]

-- | Generate the function for parsing integers
getIntegerFunction :: Doc
getIntegerFunction = vcat [
  text "",
  text "def integer: Parser[INTEGER] = {",
  nest 4 $ text "\"[0-9]+\".r ^^ {i => INTEGER(i)}",
  text "}"
  ]

-- | Generate the function for parsing double values
getDoubleFunction :: Doc
getDoubleFunction = vcat [
  text "",
  text "def double: Parser[Double] = {",
  nest 4 $ text "\"[0-9]+.[0-9]+\".r ^^ {i => Double(i)}",
  text "}"
  ]

-- | Generate the function for parsing string literals
getLiteralsFunction :: Doc
getLiteralsFunction = vcat [
  text "",
  text "def string: Parser[STRING] = {",
  nest 4 $ text "\"\\\"[^\\\"]*\\\"\".r ^^ { str =>",
  nest 8 $ text "val content = str.substring(1, str.length - 1)",
  nest 8 $ text "STRING(content)",
  nest 4 $ text "}",
  text "}"
  ]

-- | Generate the function for parsing character literals
getLiteralFunction :: Doc
getLiteralFunction = vcat [
  text "",
  text "def char: Parser[CHAR] = {",
  nest 4 $ text "\"\\\'[^\\\']*\\\'\".r ^^ { str =>",
  nest 8 $ text "val content = str.substring(1, str.length - 1)",
  nest 8 $ text "CHAR(content)",
  nest 4 $ text "}",
  text "}"
  ]

-- | Get a symbol name from a string
getSymbFromName :: String -> String
getSymbFromName s = 
  case symbolToName s of
    Just s -> s
    _ -> map toUpper s

-- | Generate the tokens function
getTokensFunction :: [String] -> [Doc]
getTokensFunction symbs = [
  text "",
  text "def tokens: Parser[List[WorkflowToken]] = {",
  nest 4 $ text $ "phrase(rep1( " ++ intercalate " | " (getListSymNames symbs) ++ "))",
  text "}"
  ]

-- | Get the lowercase symbol name
getSymName :: String -> String
getSymName = toLowerString . getSymbFromName

-- | Get a list of symbol names
getListSymNames :: [String] -> [String]
getListSymNames = map getSymName

-- | Generate a keyword parser for a symbol
getBaseLexs :: String -> Doc
getBaseLexs symb = 
  text "" $+$
  text ("def " ++ getSymName symb ++ " = positioned { \"" ++ toLowerString symb ++ "\" ^^ (_ => " ++ getSymbFromName symb ++ "()) }")
