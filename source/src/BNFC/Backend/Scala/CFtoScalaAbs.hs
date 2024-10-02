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

import Control.Monad.State (State, gets, modify, evalState)

import Data.Char     ( toLower )
import Data.Either   ( lefts )
import Data.Function ( on )
import Data.List     ( groupBy, intercalate, intersperse, nub, sort )
import Data.Maybe    ( mapMaybe )
import Data.Set      ( Set )

import qualified Data.Set as Set

import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options  ( RecordPositions(..) )
import BNFC.Utils    ( (+++), unless )
import BNFC.Backend.Common.NamedVariables


-- | The result is two files (.H file, .C file)
cf2ScalaAbs
  :: CF     -- ^ Grammar.
  -> Doc    -- ^ @.H@ file, @.C@ file.
cf2ScalaAbs cf = vcat [ "ScalaAbsFile"]
