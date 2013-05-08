
{-
    BNF Converter: Named instance variables
    Copyright (C) 2004  Author:  Michael Pellauer

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


{-
   **************************************************************
    BNF Converter Module

    Description   : This module provides support for languages which need
                    named instance variables. (IE Java, C, C++) It provides
                    a data type to represent the name mapping and utility
                    functions to work with it.

                    Variables are grouped and numbered in a nice way.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

   **************************************************************
-}

module BNFC.Backend.Common.NamedVariables where

import BNFC.CF
import Data.Char (toLower)
import Data.List (nub)

type IVar = (String, Int)
--The type of an instance variable
--and a # unique to that type

type UserDef = String --user-defined types


--A symbol-mapping environment.
type SymEnv = [(String, String)]

--Converts a list of categories into their types to be used as instance variables.
getVars :: [Cat] -> [IVar]
getVars [] = []
getVars cs = foldl addVar [] (map identCat cs)
 where
  addVar vs c = addVar' vs 0 c
  addVar' []  n c = [(c, n)]
  addVar' (i@(t,x):is) n c =
    if c == t
      then if x == 0
        then (t, 1) : (addVar' is 2 c)
	else i : (addVar' is (x+1) c)
      else i : (addVar' is n c)

--Given a rule's definition, it goes through and nicely the variables by type.
numVars :: [(String, Int)] -> [Either String b] -> [Either String b]
numVars _env [] = []
numVars env ((Right f) : fs) = (Right f) : (numVars env fs)
numVars env ((Left f) : fs) =
   case lookup f' env of
     Nothing -> (Left f') : (numVars ((f',1):env) fs)
     Just n -> (Left $ f' ++ (show $ n + 1)) : (numVars ((f',n+1):env) fs)
 where
   f' = varName (normCat (identCat f))

--This makes numbers a little nicer.
--If there's only one variable of a type we drop the redundant _1 label.
--(Actually here we add _1 labels to variables that need it, but the effect
-- is the same.)
fixOnes [] = []
fixOnes ((Right f): fs) = (Right f) : (fixOnes fs)
fixOnes ((Left f) : fs) =
  if elem (Left (f ++ "2")) fs
    then (Left (f ++ "1")) : (fixOnes fs)
    else (Left f) : (fixOnes fs)

--This fixes the problem with coercions.
fixCoercions :: [(Cat, [Rule])] -> [(Cat, [Rule])]
fixCoercions rs = nub (fixAll rs rs)
 where
  fixCoercion :: Cat -> [(Cat, [Rule])] -> [Rule]
  fixCoercion _ [] = []
  fixCoercion cat ((c,rules):cats) = if (normCat c) == (normCat cat)
    then rules ++ (fixCoercion cat cats)
    else fixCoercion cat cats
  fixAll :: [(Cat, [Rule])] -> [(Cat, [Rule])] -> [(Cat, [Rule])]
  fixAll _ [] = []
  fixAll top ((cat,rules):cats) = if isCoercion cat
    then fixAll top cats
    else (normCat cat, fixCoercion cat top) : (fixAll top cats)

--A generic variable name for C-like languages.
varName c = (map toLower c) ++ "_"

--this makes var names a little cleaner.
showNum n = if n == 0 then [] else (show n)
