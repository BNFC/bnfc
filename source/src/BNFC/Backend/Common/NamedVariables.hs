{-# LANGUAGE NoImplicitPrelude #-}

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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
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

The idea of this module is the following (if I got it correctly):

In some target languages (e.g. java or C) you need to create a variable name
for each non terminal in a given rule. For instance, the following rules:
> SomeFunction. A ::= B C D ;
could be represented in C by a structure like:

@
struct A {
  B b_;
  C c_;
  D d_;
}
@
(note that this is not exactly the representation produced by bnfc)

but if there is several non terminal of the same category, we need to number
them. Eg:
> SomeFunction. A = B B ;

Should become something like:
@
struct A {
  B b_1, b_2;
}
@

This is what this module does.

-}

module BNFC.Backend.Common.NamedVariables where

import Prelude'

import BNFC.CF
import Data.Char (toLower)
import Data.List (nub)
import Text.PrettyPrint
import Control.Arrow (left, (&&&))
import Data.Either (lefts)

type IVar = (String, Int)
--The type of an instance variable
--and a # unique to that type

type UserDef = Cat --user-defined types


--A symbol-mapping environment.
type SymEnv = [(String, String)]

-- | Converts a list of categories into their types to be used as instance
-- variables. If a category appears only once, it is given the number 0,
-- if it appears more than once, its occurrences are numbered from 1. ex:
--
-- >>> getVars [Cat "A", Cat "B", Cat "A"]
-- [("A",1),("B",0),("A",2)]
--
getVars :: [Cat] -> [IVar]
getVars [] = []
getVars cs = foldl addVar [] (map identCat cs)
  where
    addVar vs = addVar' vs 0
    addVar' []  n c = [(c, n)]
    addVar' (i@(t,x):is) n c =
      if c == t
          then if x == 0
              then (t, 1) : addVar' is 2 c
              else i : addVar' is (x+1) c
          else i : addVar' is n c

-- # Create variable names for rules rhs
-- This is about creating variable names for the right-hand side of rules.
-- In particular, if you have a rule like Foo. Bar ::= A B A, you need to
-- create unique variable names for the two instances of category A

-- | Anotate the right hand side of a rule with variable names
-- for the non-terminals.
-- >>> numVars [Left (Cat "A"), Right "+", Left (Cat "B")]
-- [Left (A,a_),Right "+",Left (B,b_)]
-- >>> numVars [Left (Cat "A"), Left (Cat "A"), Right ";"]
-- [Left (A,a_1),Left (A,a_2),Right ";"]
numVars :: [Either Cat a] -> [Either (Cat, Doc) a]
numVars cats =
  -- First, we anotate each Left _ with a variable name (not univque)
  let withNames = map (left (id &&& (varName . identCat . normCat))) cats
  -- next, the function f' adds numbers where needed...
  in f' [] withNames
  where f' _ [] = []
        f' env (Right t:xs) = Right t:f' env xs
        f' env (Left (c,n):xs) =
            -- we should use n_i as var name
            let i = maybe 1 (+1) (lookup n env)
            -- Is there more use of the name u_ ?
                thereIsMore = n `elem` map snd (lefts xs)
                vname = text n <> if i > 1 || thereIsMore then int i else empty
            in Left (c, vname) : f' ((n,i):env) xs

--This makes numbers a little nicer.
--If there's only one variable of a type we drop the redundant _1 label.
--(Actually here we add _1 labels to variables that need it, but the effect
-- is the same.)
fixOnes :: Eq b => [Either String b] -> [Either String b]
fixOnes [] = []
fixOnes (Right f : fs) = Right f : fixOnes fs
fixOnes (Left f : fs) =
  if Left (f ++ "2") `elem` fs
    then Left (f ++ "1") : fixOnes fs
    else Left f : fixOnes fs

--This fixes the problem with coercions.
fixCoercions :: [(Cat, [Rule])] -> [(Cat, [Rule])]
fixCoercions rs = nub (fixAll rs rs)
 where
  fixCoercion :: Cat -> [(Cat, [Rule])] -> [Rule]
  fixCoercion _ [] = []
  fixCoercion cat ((c,rules):cats) = if normCat c == normCat cat
    then rules ++ fixCoercion cat cats
    else fixCoercion cat cats
  fixAll :: [(Cat, [Rule])] -> [(Cat, [Rule])] -> [(Cat, [Rule])]
  fixAll _ [] = []
  fixAll top ((cat,_):cats) = if isCoercion (show cat) -- This is weird: isCoercion is supposed to be applied to functions!!!!
    then fixAll top cats
    else (normCat cat, fixCoercion cat top) : fixAll top cats

--A generic variable name for C-like languages.
varName c = map toLower c ++ "_"

--this makes var names a little cleaner.
showNum n = if n == 0 then [] else show n

-- Makes the first letter a lowercase.
firstLowerCase :: String -> String
firstLowerCase "" = ""
firstLowerCase (a:b) = toLower a:b
