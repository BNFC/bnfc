{-
    BNF Converter: Haskell error monad
    Copyright (C) 2004-2007  Author:  Markus Forsberg, Peter Gammie,
                                      Aarne Ranta, Björn Bringert
    Copyright (C) 2019 Author: Andreas Abel

-}

{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Haskell.MkErrM where

import BNFC.PrettyPrint

mkErrM :: String -> Doc
mkErrM errMod = vcat
    [ "{-# LANGUAGE CPP #-}"
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
    ]
