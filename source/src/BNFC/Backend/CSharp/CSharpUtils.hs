{-
    BNF Converter: Utility Functions for C#
    Copyright (C) 2006  Author:  Johan Broberg

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

    Description   : This module provides utility functions for the
                    C# format.

    Author        : Johan Broberg (johan@pontemonti.com)

    License       : GPL (GNU General Public License)

    Created       : 23 November, 2006

    Modified      : 21 January, 2007 by Johan Broberg

   **************************************************************
-}

module BNFC.Backend.CSharp.CSharpUtils where

import BNFC.CF
import Data.Char (toLower)
import Data.List
import BNFC.Backend.Common.OOAbstract hiding (basetypes)

type Namespace = String

--The visit-function name of a basic type
visitFunName :: String -> String
visitFunName v =
  if      "integer_" `isPrefixOf` v then "Integer"
  else if "char_"    `isPrefixOf` v then "Char"
  else if "string_"  `isPrefixOf` v then "String"
  else if "double_"  `isPrefixOf` v then "Double"
  else if "ident_"   `isPrefixOf` v then "Ident"
  else                                   "Ident" --User-defined type

isUserDefined :: String -> Bool
isUserDefined v = v `notElem` (map classVar (map fst basetypes))

basetypes = [
  ("Integer","int"),
  ("Char",   "char"),
  ("Double", "double"),
  ("String", "string"),
  ("Ident",  "string")
  ]

typename :: String -> String
typename name
  | name == "Char"    = "char"
  | name == "Double"  = "double"
  | name == "Ident"   = "string"
  | name == "Integer" = "int"
  | name == "String"  = "string"
  | otherwise         = name

-- Creates a variable name.
-- To make sure that no reserved keyword is generated, an underscore is added at the end. Not very pretty, but effective.
varname :: String -> String
varname name = (map toLower name) ++ "_"

-- Given a variable name (in an abstract syntax class), returns ".ToString()" if the name doesn't match one of the basetypes.
toString :: String -> String
toString v = if isUserDefined v then ".ToString()" else ""

-- Prepends namespace ".Absyn." to typ unless it is one of the basetypes
identifier :: Namespace -> String -> String
identifier namespace typ
  | typ `elem` (map snd basetypes) = typ
  | otherwise                      = namespace ++ ".Absyn." ++ typ

-- Removes empty lines, and removes the line-break at the end.
-- This can be useful if you want to use unlines "inside" unlines and don't want a whole lot of "useless" line-breaks.
unlinesInline :: [String] -> String
unlinesInline xs = concat $ intersperse "\n" $ filter (\x -> x /= "") xs

unlinesInlineMap :: (a -> String) -> [a] -> String
unlinesInlineMap fun xs = unlinesInline $ intersperse " " $ filter (\x -> x /= "") $ map fun xs

--Helper function that escapes characters in strings
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : ('\\' : (escapeChars xs))
escapeChars ('\"':xs) = '\\' : ('\"' : (escapeChars xs))
escapeChars (x:xs) = x : (escapeChars xs)

isAlsoCategory :: Fun -> String -> Bool
isAlsoCategory f c = f == c

flattenSignatures :: CAbs -> [(String, CSharpAbsRule)]
flattenSignatures cabs = [(c,r) | (c,rs) <- signatures cabs, r <- map cabsrule2csharpabsrule rs]

type VariableName = String
type PropertyName = String

-- Just like CAbsRule in OOAbstract, except this also has PropertyName.
-- (valcat,(constr,args)), True = is class (not basic), class variable stored
type CSharpAbsRule = (Fun,[(String,Bool,VariableName,PropertyName)])

cabsrule2csharpabsrule :: CAbsRule -> CSharpAbsRule
cabsrule2csharpabsrule (f, cabsrule) = (f, addPropertyNames cabsrule)

-- This generates names for properties. It's done the same way as generation of variable names in OOAbstract->cf2cabs
-- A property name uses the same casing as its category, but has an underscore at the end
addPropertyNames :: [(String, Bool, String)] -> [(String, Bool, VariableName, PropertyName)]
addPropertyNames cs = [(c,b,v,p) | ((c,b,v),p) <- zip cs (properties [] (map propertyName [c | (c,_,_) <- cs]))]
  --- creating new names is quadratic, but parameter lists are short
  --- this should conform with Michael's naming
  where
    properties seen vv = case vv of
      []   -> vv
      v:vs -> case length (filter (==v) seen) of
        0 | elem v vs -> (v ++ "1"): properties (v:seen) vs
        0             -> v         : properties (v:seen) vs
        n             -> (v ++ show (n+1)) : properties (v:seen) vs

propertyName :: String -> PropertyName
propertyName c = c ++ "_"

-- Given a rule's definition, it goes through and nicely the properties by type.
-- Does the same thing as numVars in NamedVariables, except the varName part
numProps :: [(String, Int)] -> [Either Cat b] -> [Either String b]
numProps _env [] = []
numProps env ((Right f) : fs) = (Right f) : (numProps env fs)
numProps env ((Left f) : fs) =
   case lookup f' env of
     Nothing -> (Left f') : (numProps ((f',1):env) fs)
     Just n -> (Left $ f' ++ (show $ n + 1)) : (numProps ((f',n+1):env) fs)
 where
   f' = propertyName (identCat (normCat f))
