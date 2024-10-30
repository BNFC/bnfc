{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Scala Abstract syntax
    Copyright (Scala) 2024  Author:  Juan Pablo Poittevin

    Description   : This module generates the Scala Abstract Syntax
                    tree classes. It generates both a Header file
                    and an Implementation file

    Author        : Juan Pablo Poittevin
    Created       : 30 September, 2024
-}

module BNFC.Backend.Scala.CFtoScalaAbs (cf2ScalaAbs) where

import Prelude hiding ((<>))

import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options
import Data.List

-- | The result is two files (.H file, .C file)
cf2ScalaAbs
  :: SharedOptions     
  -> CF     -- ^ Grammar.
  -> Doc    -- ^ @.H@ file, @.C@ file.
cf2ScalaAbs Options{ lang } cf = vsep . concat $
  [ 
      []
    , ["package" <+> text lang]
    , [hang classSign 4 $ functions]
    , defaultFunction lang
    , ["}"]
    -- , [text $ concat $ map dataToString datas]
  ]
  where
    datas     = cf2data cf
    functions = vcat (map prData datas)


dataToString :: (Cat, [(String, [Cat])]) -> String
dataToString (cat, []) = catToStr cat
dataToString (cat, cats) = "Main Cat:" ++ catToStr cat ++ " List of [(String, [Cat])]: " ++ strCatToString cats


strCatToString :: [(String, [Cat])] -> String
strCatToString ([]) = ""
strCatToString (ncat:[]) = " \n Sub Cat: " ++ fst ncat ++ " Is compose of: " ++ intercalate " " (map catToStr (snd ncat))
strCatToString (ncat:cats) = " \n  Sub Cat: " ++ fst ncat ++ " Is compose of: " ++ intercalate " " (map catToStr (snd ncat)) ++  strCatToString cats

classSign :: Doc
classSign = "abstract class AbstractVisitor[R, A] extends AllVisitor[R, A]{"

prData ::  Data -> Doc
prData (_, rules) = vcat $ concat
  [ 
    [constructors rules]
  ]
  where
    prRule (fun, _)   = hsep $ concat [ ["def visit(p: " <+> text ("Absyn." ++ fun) <+> text ", arg: A): R = visitDefault(p, arg)"] ]
    constructors []      = empty
    constructors (h:t)   = sep $ [prRule h] ++ map prRule t
  
defaultFunction :: String -> [Doc]
defaultFunction lang = 
  [
      nest 4 $ text $ "def visitDefault(p: Absyn." ++ lang ++ ", arg: A): R = {"
    , nest 8 "throw new IllegalArgumentException(this.getClass.getName + \": \" + p)"
    , nest 4 "}"
  ]