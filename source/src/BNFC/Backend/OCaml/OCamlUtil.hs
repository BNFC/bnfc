{-
    BNF Converter: OCaml backend utility module
    Copyright (C) 2005  Author:  Kristofer Johannisson

-}

{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.OCaml.OCamlUtil where

import Data.Char (toLower)

import BNFC.CF
import BNFC.Options
import BNFC.Utils

-- | Name of the parser generator.

class OCamlParserName a where
  ocamlParserName :: a -> String

instance OCamlParserName OCamlParser where
  ocamlParserName = \case
    OCamlYacc -> "ocamlyacc"
    Menhir    -> "menhir"

instance OCamlParserName SharedOptions where
  ocamlParserName = ocamlParserName . ocamlParser

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
fixTypeUpper = capitalize . fixType

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

-- | Avoid clashes with keywords.
sanitizeOcaml :: String -> String
sanitizeOcaml s
  | s `elem` reservedOCaml = s ++ "_"
  | otherwise = s

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
