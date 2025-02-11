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

module BNFC.Backend.Scala.CFtoScalaLex (cf2ScalaLex) where

import Prelude hiding ((<>))

import BNFC.Utils (symbolToName)
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options
import BNFC.Backend.Common (unicodeAndSymbols)
import Data.List
import Data.Char (toLower)

toLowerString :: String -> String
toLowerString s = map toLower s

-- | The result is two files (.H file, .C file)
cf2ScalaLex
  :: SharedOptions     
  -> CF     -- ^ Grammar.
  -> Doc    -- ^ @.H@ file, @.C@ file.
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
        | liter == catIdent   = getIdentifiersFunction
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
    , nest 4 "override def skipWhitespace = true"
    -- In case the lenguage use indentation for block creation, we should remove \n from the list of whiteSpace.
    , nest 4 $ text $ "override val whiteSpace = " ++ "\"" ++ "[\\t\\r\\f\\n]+\".r"
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
      "def identifier: Parser[IDENTIFIER] = {"
    , nest 4 "\"[a-zA-Z_][a-zA-Z0-9_]*\".r ^^ { str => IDENTIFIER(str) }"
    , "}"
  ]

getIntegerFunction :: Doc
getIntegerFunction = vcat [
      "def integer: Parser[Integer] = {"
    , nest 4 "\"[0-9]+\".r ^^ {i => Integer(i)}"
    , "}"
  ]

getLiteralsFunction :: [Doc]
getLiteralsFunction = [
      "def literal: Parser[LITERAL] = {"
    , nest 4 "\"\"\"\"[^\"]*\"\"\"\".r ^^ { str =>"
    , nest 6 "val content = str.substring(1, str.length - 1)"
    , nest 6 "LITERAL(content)"
    , nest 4 "}"
    , "}"
  ]

getIndentationsFunction :: [Doc]
getIndentationsFunction = [
    "def indentation: Parser[INDENTATION] = {"
  , nest 4 "\"\\n[ ]*\".r ^^ { whitespace =>"
  , nest 6 "val nSpaces = whitespace.length - 1"
  , nest 6 "INDENTATION(nSpaces)"
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
    -- TODO: Are the symbs enough for the token lexing? Check if we need to add any other data
    -- At the end of this list of symbs, should be identifiers (if the lenguage accept identifiers), 
    , nest 4 $ text $ "phrase(rep1( " ++ intercalate " | " (getListSymNames symbs) ++ "))"
    -- next line is needed in case of indentation use
    -- , nest 4 $ text $ "phrase(rep1(" ++ intercalate " | " (map (\s -> "\"" ++ s ++ "\"") symbs) ++ ")) ^^ { rawTokens =>"
    -- , nest 8 "processIndent(rawTokens)"
    , "}"
  ]


getSymName :: String -> String
getSymName = toLowerString.getSymbFromName

getListSymNames :: [String] -> [String]
getListSymNames symbs = map getSymName symbs

getKeywordParsers :: [String] -> [Doc]
getKeywordParsers symbs = map getKeywordParser symbs


-- def plus          = positioned { "+"      ^^ (_ => PLUS())  }
getKeywordParser :: String -> Doc
getKeywordParser symb = text $ "def " ++ getSymName symb ++ " = positioned { \"" ++ toLowerString symb ++ "\" ^^ (_ =>"++ getSymbFromName symb ++"()) }"
-- def+= +^^ (_ =>+())

-- following functino must be added only in case of acepting indentation as block separetion
-- getProcessIndentFunction :: [Doc]
-- getProcessIndentFunction = [
--   "private def processIndent(tokens: List[WorkflowToken], indents: List[Int] = List(0)): List[WorkflowToken] = {"
--    nest 4 "tokens.headOption match {"

--       case Some(INDENTATION(spaces)) if spaces > indents.head =>
--         INDENT() :: processIndentations(tokens.tail, spaces :: indents)

--       // if there is a decrease, we pop from the stack until we have matched the new level and
--       // we produce a DEDENT for each pop
--       case Some(INDENTATION(spaces)) if spaces < indents.head =>
--         val (dropped, kept) = indents.partition(_ > spaces)
--         (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

--       // if the indentation level stays unchanged, no tokens are produced
--       case Some(INDENTATION(spaces)) if spaces == indents.head =>
--         processIndentations(tokens.tail, indents)

--       // other tokens are ignored
--       case Some(token) =>
--         token :: processIndentations(tokens.tail, indents)

--       // the final step is to produce a DEDENT for each indentation level still remaining, thus
--       // "closing" the remaining open INDENTS
--       case None =>
--         indents.filter(_ > 0).map(_ => DEDENT())

--     }
--   }
--   ]