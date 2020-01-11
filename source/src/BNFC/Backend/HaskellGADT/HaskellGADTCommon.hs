{-
    BNF Converter: Haskell GADT back-end common stuff
    Copyright (C) 2004-2005  Author:  Markus Forberg, Björn Bringert

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

module BNFC.Backend.HaskellGADT.HaskellGADTCommon (Constructor(..), cf2cons, isTreeType) where

import BNFC.CF
import BNFC.Backend.Haskell.Utils ( catToVar )


data Constructor = Constructor
    { consCat :: Cat
    , consFun :: Fun
    , consPrec :: Integer
    , consVars :: [(Cat,String)]
    , consRhs :: [Either Cat String]
    }

-- | Get category, function, and rhs categories paired with variable names.
cf2cons :: CF -> [Constructor]
cf2cons cf =
    [  Constructor
        { consCat = cat, consFun = fun, consPrec = precFun cf fun
        , consVars = zip cats (mkVars cats), consRhs = rhsFun cf fun
        } | (cat,rules) <- cf2data cf, (fun,cats) <- rules]
    ++ [ Constructor
        { consCat = TokenCat cat, consFun = cat, consPrec = 0
        , consVars = [(Cat "String","str")], consRhs = [Left (Cat "String")]
        } | cat <- specialCats cf]
  where
    mkVars cats = mkUnique (map catToVar cats) (0 :: Int)
    mkUnique [] _ = []
    mkUnique (x:xs) n | x `elem` xs || n > 0 = (x ++ show n) : mkUnique xs (n+1)
                      | otherwise = x : mkUnique xs n

-- | Get the rule for a function.
ruleFun :: CF -> Fun -> Rule
ruleFun cf f = head $ filter (\r -> funRule r == f) $ cfgRules cf

-- | Get the precedence of a function.
precFun :: CF -> Fun -> Integer
precFun cf f = precRule $ ruleFun cf f

-- | Get the RHS of a function
rhsFun :: CF -> Fun -> [Either Cat String]
rhsFun cf f = rhsRule $ ruleFun cf f

isTreeType :: CF -> Cat -> Bool
isTreeType cf (TokenCat c) = c `elem` specialCats cf
isTreeType cf c
  | isList c  = isTreeType cf (catOfList c)
  | otherwise = c `elem` reallyAllCats cf
