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

-- | The result is two files (.H file, .C file)
cf2ScalaLex
  :: SharedOptions     
  -> CF     -- ^ Grammar.
  -> Doc    -- ^ @.H@ file, @.C@ file.
cf2ScalaLex Options{ lang } cf = vsep . concat $
  [ 
      []
    , imports
    , initWorkflowClass
    , getApplyFunction
    , getTokensFunction symbs
    , getIdentifiersFunction -- This should be called only in case of having identifiers in the lenguage, which anyway will be the case in most of the lenguages.
    , getLiteralsFunction
    , getIndentationsFunction
    , endWorkflowClass
  ]
  where
    symbs     = unicodeAndSymbols cf


imports :: [Doc]
imports  = [
   "package co.enear.parsercombinators.lexer"
  , "import co.enear.parsercombinators.compiler.{Location, WorkflowLexerError}"
  , "import scala.util.parsing.combinator.RegexParsers"
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
    , nest 8 "case NoSuccess(msg, next) => Left(WorkflowLexerError(Location(next.pos.line, next.pos.column), msg))"
    , nest 8 "case Success(result, next) => Right(result)"
    , nest 4"}"
    , "}"
  ]


getIdentifiersFunction :: [Doc]
getIdentifiersFunction = [
      "def identifier: Parser[IDENTIFIER] = {"
    , nest 4 "\"[a-zA-Z_][a-zA-Z0-9_]*\".r ^^ { str => IDENTIFIER(str) }"
    , "}"
  ]

getLiteralsFunction :: [Doc]
getLiteralsFunction = [
      "def literal: Parser[LITERAL] = {"
    , nest 4 "\"\"\"\"[^\"]*\"\"\"\".r ^^ { str =>"
    , nest 8 "val content = str.substring(1, str.length - 1)"
    , nest 8 "LITERAL(content)"
    , nest 4 "}"
    , "}"
  ]

getIndentationsFunction :: [Doc]
getIndentationsFunction = [
    "def indentation: Parser[INDENTATION] = {"
  , nest 4 "\"\\n[ ]*\".r ^^ { whitespace =>"
  , nest 8 "val nSpaces = whitespace.length - 1"
  , nest 8 "INDENTATION(nSpaces)"
  , nest 4 "}"
  , "}"
  ]


getTokensFunction :: [String] -> [Doc]
getTokensFunction symbs = [
     "def tokens: Parser[List[WorkflowToken]] = {"
    -- TODO: Are the symbs enough for the token lexing? Check if we need to add any other data
    -- At the end of this list of symbs, should be identifiers (if the lenguage accept identifiers), 
    , nest 4 $ text $ "phrase(rep1(" ++ intercalate " | " (map (\s -> "\"" ++  s ++ "\"")  symbs) ++ "))"
    -- next line is needed in case of indentation use
    -- , nest 4 $ text $ "phrase(rep1(" ++ intercalate " | " (map (\s -> "\"" ++ s ++ "\"") symbs) ++ ")) ^^ { rawTokens =>"
    -- , nest 8 "processIndent(rawTokens)"
    , "}"
  ]

-- getListSymNames :: [String] -> [String]
-- getListSymNames symbs = map ( . symbolToName) symbs

getKeywordParser :: String -> [Doc]
getKeywordParser symb = [
    text $ "def" ++ symb  ++ "= " ++ symb ++ "^^ (_ => EXIT)"
  ]


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