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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module BNFC.Backend.HaskellGADT.HaskellGADTCommon (Constructor(..), cf2cons, isTreeType) where

import BNFC.CF

import Data.Char

data Constructor = Constructor {
				consCat :: Cat,
				consFun :: Fun,
				consPrec :: Integer,
				consVars :: [(Cat,String)],
				consRhs :: [Either Cat String]
			       }

-- | Get category, function, and rhs categories paired with variable names.
cf2cons :: CF -> [Constructor]
cf2cons cf = [  Constructor { consCat = cat,
			      consFun = fun,
			      consPrec = precFun cf fun,
			      consVars = zip cats (mkVars cats),
			      consRhs = rhsFun cf fun}
		| (cat,rules) <- cf2data cf, (fun,cats) <- rules]
	     ++ [ Constructor { consCat = cat,
				consFun = show cat,
				consPrec = 0,
				consVars = [(Cat "String","str")],
				consRhs = [Left (Cat "String")]}
		  | cat <- specialCats cf]
 where mkVars cats = mkUnique (map catToVar cats) (0 :: Int)
       mkUnique [] _ = []
       mkUnique (x:xs) n | elem x xs || n > 0 = (x ++ show n) : mkUnique xs (n+1)
			 | otherwise = x : mkUnique xs n

-- | Make a variable name for a category.
catToVar :: Cat -> String
catToVar = checkRes . var
  where var (ListCat cat) = var cat ++ "s"
        var (Cat "Ident")   = "i"
        var (Cat "Integer") = "n"
        var (Cat "String")  = "str"
        var (Cat "Char")    = "c"
        var (Cat "Double")  = "d"
        var xs        = map toLower $show xs
        checkRes s | elem s reservedHaskell = s ++ "'"
		   | otherwise              = s
	reservedHaskell = ["case","class","data","default","deriving","do","else","if",
			   "import","in","infix","infixl","infixr","instance","let","module",
			   "newtype","of","then","type","where","as","qualified","hiding"]

-- | Get the rule for a function.
ruleFun :: CF -> Fun -> Rule
ruleFun cf f = head $ filter (\r -> funRule r == f) $ rulesOfCF cf

-- | Get the precedence of a function.
precFun :: CF -> Fun -> Integer
precFun cf f = precRule $ ruleFun cf f

-- | Get the RHS of a function
rhsFun :: CF -> Fun -> [Either Cat String]
rhsFun cf f = rhsRule $ ruleFun cf f

isTreeType :: CF -> Cat -> Bool
isTreeType cf c | isList c  = isTreeType cf (catOfList c)
                | otherwise = c `elem` (allCats cf ++ specialCats cf)
