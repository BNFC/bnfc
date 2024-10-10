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
import Data.List (intercalate)

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

mapBut1 :: (a -> a) -> [a] -> [a]
mapBut1 f (x:y:xs) = f x : mapBut1 f (y:xs)
mapBut1 _ other = other

prData :: Bool -> Data -> Doc
prData functor (cat,rules) = vcat $ concat
  [ 
    [ hang ("Sclass" <+> dataType <+> ":") 4 $ constructors rules ] -- I have to find a way to add strings without space, <+> add stirngs but add also a space
  ]
  where
    -- This "mapBut1" is used to add a comma between numerated parameters, but I think should be a better way
    prRule (fun, cats)   = hsep $ concat [ [text fun <+> "("], mapBut1 (<+> text ", ") (map prArg (indexedCats cats)) , [text ") {}"] ]
    dataType             = pretty cat
    indexedCats c        = zip [0..] c
    prArg (n, c)         = text $ catToStr c ++ " " ++ ("p" ++ show n) 
    constructors []      = empty
    constructors (h:t)   = sep $ [prRule h] ++ map prRule t