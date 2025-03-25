{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Scala Lextract syntax
    Copyright (Scala) 2024  Author:  Juan Pablo Poittevin

    Description   : This module generates the Scala Lextract Syntax
                    tree classes. Using Scala Parser Combinator
    Author        : Juan Pablo Poittevin
    Created       : 30 September, 2024
-}

module BNFC.Backend.Scala.CFtoScalaLex (cf2ScalaLex) where

import Prelude hiding ((<>))

import BNFC.Utils (symbolToName)
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options
import BNFC.Backend.Common (unicodeAndSymbols)
import Data.List ( intercalate )
import Data.Char (toLower)

toLowerString :: String -> String
toLowerString s = map toLower s

cf2ScalaLex
  :: SharedOptions     
  -> CF     -- Grammar.
  -> Doc    
cf2ScalaLex Options{ lang } cf = vsep . concat $
  [ 
      []
    , imports lang
    , addExtraClasses
    , initWorkflowClass
    , map (nest 4) getApplyFunction
    , map (nest 4) (getTokensFunction (symbs ++ liters))
    -- , map (nest 4) getIdentifiersFunction -- This should be called only in case of having identifiers in the lenguage, which anyway will be the case in most of the lenguages.
    , map (nest 4) (addParserFunctions liters)
    -- , map (nest 4) getLiteralsFunction
    -- , getIndentationsFunction
    , map (nest 4) (getKeywordParsers symbs)
    , endWorkflowClass
  ]
  where
    symbs     = unicodeAndSymbols cf
    liters    = literals cf

-- catString, catInteger, catDouble, catChar, catIdent :: TokenCat
-- catString  = "String"
-- catInteger = "Integer"
-- catDouble  = "Double"
-- catChar    = "Char"
-- catIdent   = "Ident"

addParserFunctions :: [String] -> [Doc]
addParserFunctions liters = map addParserFunction liters

addParserFunction :: String -> Doc
addParserFunction liter 
        | liter == catInteger = getIntegerFunction
        | liter == catDouble  = getDoubleFunction
        | liter == catIdent   = getIdentifiersFunction
        | liter == catString  = getLiteralsFunction
        | liter == catChar    = getLiteralFunction
        | otherwise = ""

imports :: String -> [Doc]
imports name  = [
   text $ "package " ++ name ++ ".workflowtoken." ++ name ++ "Lex"
  , "import scala.util.parsing.combinator.RegexParsers"
  ]

addExtraClasses :: [Doc]
addExtraClasses = [
  "sealed trait WorkflowCompilationError"
  ,"case class WorkflowLexerError(location: Location, msg: String) extends WorkflowCompilationError"
  ,"case class WorkflowParserError(location: Location, msg: String) extends WorkflowCompilationError"
  ,"case class Location(line: Int, column: Int) {"
  ,nest 4 "override def toString = s\"$line:$column\""
  ,"}"
  ]


initWorkflowClass :: [Doc]
initWorkflowClass = [
      "object WorkflowLexer extends RegexParsers {"
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
      "def apply(code: String): Either[WorkflowLexerError, List[WorkflowToken]] = {"
    , nest 4 "parse(tokens, code) match {"
    , nest 6 "case NoSuccess(msg, next) => Left(WorkflowLexerError(Location(next.pos.line, next.pos.column), msg))"
    , nest 6 "case Success(result, next) => Right(result)"
    , nest 4"}"
    , "}"
  ]


getIdentifiersFunction :: Doc
getIdentifiersFunction = vcat [
      "def ident: Parser[IDENT] = {"
    , nest 4 "\"[a-zA-Z_][a-zA-Z0-9_]*\".r ^^ { str => IDENT(str) }"
    , "}"
  ]

getIntegerFunction :: Doc
getIntegerFunction = vcat [
      "def integer: Parser[INTEGER] = {"
    , nest 4 "\"[0-9]+\".r ^^ {i => INTEGER(i)}"
    , "}"
  ]


getDoubleFunction :: Doc
getDoubleFunction = vcat [
      "def double: Parser[Double] = {"
    , nest 4 "\"[0-9]+.[0-9]+\".r ^^ {i => Double(i)}"
    , "}"
  ]


getLiteralsFunction :: Doc
getLiteralsFunction = vcat [
      "def string: Parser[STRING] = {"
    , nest 4 "\"\\\"[^\\\"]*\\\"\".r ^^ { str =>"
    , nest 6 "val content = str.substring(1, str.length - 1)"
    , nest 6 "STRING(content)"
    , nest 4 "}"
    , "}"
  ]

getLiteralFunction :: Doc
getLiteralFunction = vcat [
      "def char: Parser[CHAR] = {"
    , nest 4 "\"\\\'[^\\\']*\\\'\".r ^^ { str =>"
    , nest 6 "val content = str.substring(1, str.length - 1)"
    , nest 6 "CHAR(content)"
    , nest 4 "}"
    , "}"
  ]


getSymbFromName :: String -> String
getSymbFromName s = 
  case symbolToName s of
  Just s -> s
  _ -> s


getTokensFunction :: [String] -> [Doc]
getTokensFunction symbs = [
     "def tokens: Parser[List[WorkflowToken]] = {" 
    , nest 4 $ text $ "phrase(rep1( " ++ intercalate " | " (getListSymNames symbs) ++ "))"
    , "}"
  ]


getSymName :: String -> String
getSymName = toLowerString.getSymbFromName

getListSymNames :: [String] -> [String]
getListSymNames symbs = map getSymName symbs

getKeywordParsers :: [String] -> [Doc]
getKeywordParsers symbs = map getKeywordParser symbs


getKeywordParser :: String -> Doc
getKeywordParser symb = text $ "def " ++ getSymName symb ++ " = positioned { \"" ++ toLowerString symb ++ "\" ^^ (_ =>"++ getSymbFromName symb ++"()) }"
