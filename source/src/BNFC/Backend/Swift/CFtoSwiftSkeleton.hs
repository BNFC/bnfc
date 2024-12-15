{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Swift.CFtoSwiftSkeleton (cf2SwiftSkeleton) where

import Data.Bifunctor (Bifunctor(second))
import Data.List (intercalate, intersperse)
import Data.Maybe (mapMaybe)

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)

import BNFC.Utils ((+++), camelCase_)
import BNFC.CF
import BNFC.Backend.Swift.Common
import BNFC.Options (SharedOptions (lang))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (antlrRuleLabel, makeLeftRecRule)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)

cf2SwiftSkeleton :: String -> CF -> SharedOptions -> Doc
cf2SwiftSkeleton packageName cf opts = vcat $ intersperse (text "")
    [ importDecls
    -- , errorsDecl
    -- , tokenDecls
    -- , buildFnDecls
    ]
  where
    importDecls :: Doc
    importDecls = vcat
      [ "import Foundation"
      , text $  "import" +++ packageName
      ]
   