{-
    BNF Converter: Named instance variables
    Copyright (C) 2004  Author:  Michael Pellauer

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

import Control.Arrow (left, (&&&))
import Data.Char     (toLower, toUpper)
import Data.Either   (lefts)
import Data.List     (nub)
import Data.Map      (Map)

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

import BNFC.CF

type IVar = (String, Int)
--The type of an instance variable
--and a # unique to that type

type UserDef = TokenCat --user-defined types

-- | A symbol-mapping environment.
type SymEnv = KeywordEnv

-- | Map keywords to their token name.
type KeywordEnv = [(String, String)]

-- | Map keywords and user-defined token types to their token name.
type SymMap = Map SymKey String
data SymKey
  = Keyword String    -- ^ Keyword like "(", "while", "true", ...
  | Tokentype String  -- ^ Token type like "Integer", "Char", ...
  deriving (Eq, Ord, Show)

-- | Converts a list of categories into their types to be used as instance
-- variables. If a category appears only once, it is given the number 0,
-- if it appears more than once, its occurrences are numbered from 1. ex:
--
-- >>> getVars [Cat "A", Cat "B", Cat "A"]
-- [("A",1),("B",0),("A",2)]
--
getVars :: [Cat] -> [IVar]
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
                vname = P.text n P.<> if i > 1 || thereIsMore then P.int i else P.empty
            in Left (c, vname) : f' ((n,i):env) xs


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
  fixAll top ((cat,_):cats) = if isCoercion (noPosition $ catToStr cat) -- This is weird: isCoercion is supposed to be applied to functions!!!!
    then fixAll top cats
    else (normCat cat, fixCoercion cat top) : fixAll top cats

--A generic variable name for C-like languages.
varName :: String -> String
varName c = map toLower c ++ "_"

--this makes var names a little cleaner.
showNum :: Int -> String
showNum n = if n == 0 then "" else show n

-- Makes the first letter a lowercase.
firstLowerCase :: String -> String
firstLowerCase "" = ""
firstLowerCase (a:b) = toLower a:b

firstUpperCase :: String -> String
firstUpperCase "" = ""
firstUpperCase (a:b) = toUpper a:b