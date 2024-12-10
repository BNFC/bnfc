
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

module BNFC.Backend.Scala.CFtoScalaLexToken (cf2ScalaLexToken) where

import Prelude hiding ((<>))

import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options
import BNFC.Backend.Common (unicodeAndSymbols)

cf2ScalaLexToken
  :: SharedOptions     
  -> CF
  -> Doc
cf2ScalaLexToken Options{ lang } cf = vsep . concat $
  [ 
      []
    , headers
    , [text $ concat $ map generateSymbClass symbs]
  ]
  where
    datas     = cf2data cf
    symbs = unicodeAndSymbols cf


generateSymbClass :: String -> String
generateSymbClass symb = "case class " ++ symb ++ "() extends WorkflowToken \n"

headers :: [Doc]
headers  = [
   "package co.enear.parsercombinators.lexer"
  , "import scala.util.parsing.input.Positional"
  , "sealed trait WorkflowToken extends Positional"
  ]
