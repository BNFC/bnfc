{-
    BNF Converter: OCaml backend utility module
    Copyright (C) 2005  Author:  Kristofer Johannisson

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

{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.OCaml.OCamlUtil where

import BNFC.CF
import BNFC.Utils
import Data.Char (toLower, toUpper)

-- Translate Haskell types to OCaml types
-- Note: OCaml (data-)types start with lowercase letter
fixType :: Cat -> String
fixType = fixTypeQual ""

fixTypeQual :: String -- ^ Module name (or empty string for no qualification).
  -> Cat -> String
fixTypeQual m = \case
  ListCat c -> fixTypeQual m c +++ "list"
  -- unqualified base types
  TokenCat "Integer" -> "int"
  TokenCat "Double"  -> "float"
  TokenCat "String"  -> "string"
  TokenCat "Char"    -> "char"
  cat -> if null m then base else concat [ m, ".", base ]
    where
    c:cs = identCat cat
    ls   = toLower c : cs
    base = if ls `elem` reservedOCaml then ls ++ "T" else ls

-- as fixType, but leave first character in upper case
fixTypeUpper :: Cat -> String
fixTypeUpper c = case fixType c of
    [] -> []
    c:cs -> toUpper c : cs


reservedOCaml :: [String]
reservedOCaml = [
    "and","as","assert","asr","begin","class",
    "constraint","do","done","downto","else","end",
    "exception","external","false","for","fun","function",
    "functor","if","in","include","inherit","initializer",
    "land","lazy","let","list","lor","lsl","lsr",
    "lxor","match","method","mod","module","mutable",
    "new","nonrec","object","of","open","or",
    "private","rec","sig","struct","then","to",
    "true","try","type","val","virtual","when",
    "while","with"]

-- | Keywords of @ocamllex@.
reservedOCamlLex :: [String]
reservedOCamlLex =
  [ "and"
  , "as"
  , "eof"
  , "let"
  , "parse"
  , "refill"
  , "rule"
  , "shortest"
  ]

-- | Heuristics to produce name for ocamllex token definition that
-- does not clash with the ocamllex keywords.
ocamlTokenName :: String -> String
ocamlTokenName x0
  | x `elem` reservedOCamlLex = x ++ "_"
  | otherwise                 = x
  where x = mapHead toLower x0

mkTuple :: [String] -> String
mkTuple [] = ""
mkTuple [x] = x
mkTuple (x:xs) = "(" ++ foldl (\acc e -> acc ++ "," +++ e) x xs ++ ")"

insertBar :: [String] -> [String]
insertBar [] = []
insertBar [x]    = ["    " ++ x]
insertBar (x:xs) = ("    " ++ x ) :  map ("  | " ++) xs

mutualDefs :: [String] -> [String]
mutualDefs defs = case defs of
     []   -> []
     [d]  -> ["let rec" +++ d]
     d:ds -> ("let rec" +++ d) : map ("and" +++) ds

-- | Escape @"@ and @\@.  TODO: escape unprintable characters!?
mkEsc :: String -> String
mkEsc s = "\"" ++ concatMap f s ++ "\""
  where
  f x = if x `elem` ['"','\\'] then "\\" ++ [x] else [x]
