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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module STLUtils where

import Data.Char

nsDefine :: Maybe String -> String -> String
nsDefine inPackage h = maybe h (\ns -> map toUpper ns ++ "_" ++ h) inPackage

nsStart :: Maybe String -> String
nsStart inPackage = maybe "" (\ns -> "namespace " ++ ns ++ "\n{") inPackage

nsEnd :: Maybe String -> String
nsEnd inPackage = maybe "" (\ns -> "}") inPackage

nsScope :: Maybe String -> String
nsScope inPackage = maybe "" (\ns -> ns ++ "::") inPackage

nsString :: Maybe String -> String
nsString inPackage = maybe "" id inPackage
