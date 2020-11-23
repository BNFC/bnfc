{-
    BNF Converter: C++ common functions
    Copyright (C) 2008  Author:  Martin Ejdestig

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
