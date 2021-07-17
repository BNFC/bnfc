{-
    BNF Converter: GADT Template Generator
    Copyright (C) 2004-2005  Author:  Markus Forsberg, Björn Bringert

-}


module BNFC.Backend.HaskellGADT.CFtoTemplateGADT (cf2Template) where

import Data.List  ( groupBy )

import BNFC.CF
import BNFC.Utils ( ModuleName, (+++) )

import BNFC.Backend.Haskell.Utils ( noWarnUnusedMatches )
import BNFC.Backend.HaskellGADT.HaskellGADTCommon

cf2Template :: ModuleName -> ModuleName -> CF -> String
cf2Template skelName absName cf = unlines $ concat
  [ [ "{-# LANGUAGE GADTs #-}"
    , "{-# LANGUAGE EmptyCase #-}"
    , ""
    , noWarnUnusedMatches
    , ""
    , "module "++ skelName ++ " where"
    , ""
    , "import Prelude (Either(..), Show(..), String, ($), (++))"
    , ""
    , "import qualified " ++ absName
    , ""
    , "type Err = Either String"
    , "type Result = Err String"
    , ""
    , "failure :: Show a => a -> Result"
    , "failure x = Left $ \"Undefined case: \" ++ show x"
    , ""
    , "transTree :: " ++ qualify "Tree" ++ " c -> Result"
    , "transTree t = case t of"
    ]
  , map prConsCase (cf2cons cf)
  , [ "" ]
  , concatMap ((++ [""]) . uncurry prCatTrans) (catCons cf)
  ]
  where
  prCatTrans :: Cat -> [Constructor] -> [String]
  prCatTrans cat cs = concat
    [ [ "trans" ++ s +++ "::" +++ qualify s +++ "-> Result"
      , "trans" ++ s +++ "t = case t of"
      ]
    , map prConsCase cs
    ]
    where
    s = catToStr cat

  prConsCase :: Constructor -> String
  prConsCase c =
    "  " ++ qualify (consFun c) +++ unwords (map snd (consVars c)) +++ "-> failure t"

  qualify x = concat [ absName, ".", x ]

catCons :: CF -> [(Cat,[Constructor])]
catCons cf = [ (consCat (head cs),cs) | cs <- groupBy catEq $ cf2cons cf]

catEq :: Constructor -> Constructor -> Bool
catEq c1 c2 = consCat c1 == consCat c2
