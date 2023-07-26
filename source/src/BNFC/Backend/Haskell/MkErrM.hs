{-
    BNF Converter: Haskell error monad
    Copyright (C) 2004-2007  Author:  Markus Forsberg, Peter Gammie,
                                      Aarne Ranta, Björn Bringert
    Copyright (C) 2019 Author: Andreas Abel

-}

{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Haskell.MkErrM where

import BNFC.PrettyPrint
import BNFC.Options (ErrorType(..))

-- | Creates @ErrM.hs@ file if needed.
--
-- It returns 'Nothing' if there is no need to create it.
mkErrM :: String -> ErrorType -> Maybe Doc
mkErrM _      ErrorTypeStructured = Nothing
   -- ErrM.hs is only for backward compatibility with old code using string
   -- errors, so that we don't create it in case of structured errors.
mkErrM errMod ErrorTypeString = Just $ vcat
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
    , ""
    , "module" <+> text errMod <+> "where"
    , ""
    , "import Prelude             (id, const, Either(..), String)"
    , ""
    , "import Control.Monad       (MonadPlus(..))"
    , "import Control.Applicative (Alternative(..))"
    , "#if __GLASGOW_HASKELL__ >= 808"
    , "import Control.Monad       (MonadFail(..))"
    , "#endif"
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
