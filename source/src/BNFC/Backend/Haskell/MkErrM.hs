{-# LANGUAGE NoImplicitPrelude #-}

{-
    BNF Converter: Haskell error monad
    Copyright (C) 2004-2007  Author:  Markus Forberg, Peter Gammie,
                                      Aarne Ranta, Björn Bringert

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}
module BNFC.Backend.Haskell.MkErrM where

import Prelude'

import BNFC.PrettyPrint

mkErrM :: String -> Bool -> Doc
mkErrM errMod ghc = vcat
    [ if ghc then "{-# LANGUAGE CPP #-}" else empty
    , "-- BNF Converter: Error Monad"
    , "-- Copyright (C) 2004  Author:  Aarne Ranta"
    , ""
    , "-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE."
    , "module " <> text errMod <> " where"
    , ""
    , "-- the Error monad: like Maybe type with error msgs"
    , ""
    , "import Control.Monad (MonadPlus(..), liftM)"
    , if ghc then "#if __GLASGOW_HASKELL__ < 710" else empty
    , "import Control.Applicative (Applicative(..), Alternative(..))"
    , if ghc then "#else" else empty
    , if ghc then "import Control.Applicative (Alternative(..))" else empty
    , if ghc then "#endif" else empty
    , ""
    , "data Err a = Ok a | Bad String"
    , "  deriving (Read, Show, Eq, Ord)"
    , ""
    , "instance Monad Err where"
    , "  return      = Ok"
    , "  fail        = Bad"
    , "  Ok a  >>= f = f a"
    , "  Bad s >>= _ = Bad s"
    , ""
    , "instance Applicative Err where"
    , "  pure = Ok"
    , "  (Bad s) <*> _ = Bad s"
    , "  (Ok f) <*> o  = liftM f o"
    , ""
    , "instance Functor Err where"
    , "  fmap = liftM"
    , ""
    , "instance MonadPlus Err where"
    , "  mzero = Bad \"Err.mzero\""
    , "  mplus (Bad _) y = y"
    , "  mplus x       _ = x"
    , ""
    , "instance Alternative Err where"
    , "  empty = mzero"
    , "  (<|>) = mplus"
    ]
