{-
    BNF Converter: Antlr4 class definition
    Copyright (C) 2016  Author:  Gabriele Paganelli

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

-------------------------------------------------------------------
-- |
-- Module      :  Antlr4
-- Copyright   :  (C)opyright 2015, gapag at distuzione dot org
-- License     :  GPL (see COPYING for details)
--
-- Maintainer  :  gapag at distuzione dot org
-- Stability   :  alpha
-- Portability :  Haskell98
--
-- Top-level for Antlr4 backend
--
-- > $Id: JavaTop15.hs,v 1.12 2007/01/08 18:20:23 aarne Exp $
-------------------------------------------------------------------

module BNFC.Backend.Antlr4 where

import Data.List
import BNFC.CF
import BNFC.Backend.Java.Utils
import BNFC.Backend.Common.NamedVariables
import BNFC.Utils ( (+++), (+.+))

type AntlrSyntaxEntity = String

class Antlr4 a where
   generateAction :: a -> AntlrSyntaxEntity
   lexerPreamble :: a -> AntlrSyntaxEntity
   parserPreamble :: a -> AntlrSyntaxEntity