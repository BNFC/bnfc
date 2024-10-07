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
import BNFC.Backend.Haskell.Utils
import Data.Either
import BNFC.Options

-- | The result is two files (.H file, .C file)
cf2ScalaAbs
  :: SharedOptions     
  -> CF     -- ^ Grammar.
  -> Doc    -- ^ @.H@ file, @.C@ file.
cf2ScalaAbs Options{ lang, tokenText, generic, functor } cf = vsep . concat $
  [ 
      []
    , map text $ map dataToString datas
    , map (prData functor) datas
    , ["// amount of datas: ", text $ show $ length datas]
    , ["// ScalaAbsFile", ""]
  ]
  where
    datas    = cf2data cf


dataToString :: (Cat, [(String, [Cat])]) -> String
dataToString (cat, _) = catToStr cat


prData :: Bool -> Data -> Doc
prData functor (cat,rules) = vcat $ concat
  [ 
    [ hang ("Sclass" <+> dataType) 4 $ constructors rules ]
  ]
  where
    prRule (fun, cats)   = hsep $ concat [ [text fun], map prArg cats ]
    dataType             = pretty cat
    prArg c              = catToType id empty c
    constructors []      = empty
    -- I need to fix the next line, is adding the : at the haskell style and not Scala
    constructors (h:t)   = sep $ [prRule h <+> ":"] ++ map ((<+>":") . prRule) t  -- TODO: this can be improved, there is no need for 2 maps
    -- constructors (h:t)   = sep $ [text h <+> ":"]  