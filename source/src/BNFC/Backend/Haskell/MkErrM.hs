{-
    BNF Converter: Haskell error monad
    Copyright (C) 2004-2007  Author:  Markus Forberg, Peter Gammie,
                                      Aarne Ranta, Björn Bringert
    Copyright (C) 2019 Author: Andreas Abel

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

import BNFC.PrettyPrint

mkErrM :: String -> Doc
mkErrM errMod = vcat
    [ "{-# LANGUAGE CPP #-}"
    , ""
    , "#if __GLASGOW_HASKELL__ >= 708"
    , "---------------------------------------------------------------------------"
    , "-- Pattern synonyms exist since ghc 7.8."
    , ""
    , "-- | BNF Converter: Error Monad."
    , "--"
    , "-- Module for backwards compatibility."
    , "--"
    , "-- The generated parser now uses @'Either' String@ as error monad."
    , "-- This module defines a type synonym 'Err' and pattern synonyms"
    , "-- 'Bad' and 'Ok' for 'Left' and 'Right'."
    , ""
    , "{-# LANGUAGE PatternSynonyms #-}"
    , "{-# LANGUAGE FlexibleInstances #-}"
    , "{-# LANGUAGE TypeSynonymInstances #-}"
    , ""
    , "module" <+> text errMod <+> "where"
    , ""
    , "import Control.Monad       (MonadPlus(..))"
    , "import Control.Applicative (Alternative(..))"
    , ""
    , "-- | Error monad with 'String' error messages."
    , "type Err = Either String"
    , ""
    , "pattern Bad msg = Left msg"
    , "pattern Ok  a   = Right a"
    , ""
    , "#if __GLASGOW_HASKELL__ >= 808"
    , "instance MonadFail Err where"
    , "  fail = Bad"
    , "#endif"
    , ""
    , "instance Alternative Err where"
    , "  empty           = Left \"Err.empty\""
    , "  (<|>) Left{}    = id"
    , "  (<|>) x@Right{} = const x"
    , ""
    , "instance MonadPlus Err where"
    , "  mzero = empty"
    , "  mplus = (<|>)"
    , ""
    , "#else"
    , "---------------------------------------------------------------------------"
    , "-- ghc 7.6 and before: use old definition as data type."
    , ""
    , "-- | BNF Converter: Error Monad"
    , ""
    , "-- Copyright (C) 2004  Author:  Aarne Ranta"
    , "-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE."
    , ""
    , "module" <+> text errMod <+> "where"
    , ""
    , "-- the Error monad: like Maybe type with error msgs"
    , ""
    , "import Control.Applicative (Applicative(..), Alternative(..))"
    , "import Control.Monad       (MonadPlus(..), liftM)"
    , ""
    , "data Err a = Ok a | Bad String"
    , "  deriving (Read, Show, Eq, Ord)"
    , ""
    , "instance Monad Err where"
    , "  return      = Ok"
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
    , ""
    , "#endif"
    ]
