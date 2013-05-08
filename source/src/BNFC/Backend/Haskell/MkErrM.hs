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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
module BNFC.Backend.Haskell.MkErrM where

errM :: String -> b -> String
errM errMod _ = unlines
	   [
	    "-- BNF Converter: Error Monad",
	    "-- Copyright (C) 2004  Author:  Aarne Ranta",
	    "",
	    "-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.",
	    "module " ++ errMod ++ " where",
	    "",
	    "-- the Error monad: like Maybe type with error msgs",
	    "",
            "import Control.Monad (MonadPlus(..), liftM)",
            "",
	    "data Err a = Ok a | Bad String",
	    "  deriving (Read, Show, Eq, Ord)",
	    "",
	    "instance Monad Err where",
	    "  return      = Ok",
	    "  fail        = Bad",
	    "  Ok a  >>= f = f a",
	    "  Bad s >>= f = Bad s",
            "",
            "instance Functor Err where",
            "  fmap = liftM",
            "",
            "instance MonadPlus Err where",
            "  mzero = Bad \"Err.mzero\"",
            "  mplus (Bad _) y = y",
            "  mplus x       _ = x"
	   ]
