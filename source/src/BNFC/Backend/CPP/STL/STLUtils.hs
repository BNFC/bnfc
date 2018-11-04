{-
    BNF Converter: C++ common functions
    Copyright (C) 2008  Author:  Martin Ejdestig

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

module BNFC.Backend.CPP.STL.STLUtils where

import Data.Char
import Data.Maybe (fromMaybe)

nsDefine :: Maybe String -> String -> String
nsDefine inPackage h = maybe h (\ns -> map toUpper ns ++ "_" ++ h) inPackage

nsStart :: Maybe String -> String
nsStart = maybe "" (\ns -> "namespace " ++ ns ++ "\n{")

nsEnd :: Maybe String -> String
nsEnd = maybe "" (const "}")

nsScope :: Maybe String -> String
nsScope = maybe "" (++ "::")

nsString :: Maybe String -> String
nsString = fromMaybe ""
