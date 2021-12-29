{-
    BNF Converter: FSharp backend utility module
    Copyright (C) 2021  Author:  Grzegorz Dziadkiewicz

-}

-- based on BNFC OCaml backend

module BNFC.Backend.FSharp.FSharpUtil where

import BNFC.CF
import BNFC.Utils
import Data.List(intercalate)
import Data.Char (toLower, toUpper)

-- Translate Haskell types to F# types
fixType :: Cat -> String
fixType (ListCat c) = fixType c +++ "list"
fixType (TokenCat "Integer") = "int"
fixType (TokenCat "Double") = "float"
fixType (TokenCat "String") = "string"
fixType (TokenCat "Char")   = "char"
fixType cat = fixKeywordUse $ show cat

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
    ls = identCat cat
    base = if ls `elem` reservedFSharp then ls ++ "T" else ls

-- as fixType, but leave first character in upper case
fixTypeUpper :: Cat -> String
fixTypeUpper c = case fixType c of
    [] -> []
    c:cs -> toUpper c : cs

fixKeywordUse:: String -> String
fixKeywordUse s = if s `elem` reservedFSharp then s ++ "T" else s

reservedFSharp :: [String]
reservedFSharp = [
    "abstract","and","as","asr","assert","base","begin","class","default","delegate","do","done",
    "downcast","downto","elif","else","end","exception","extern","false","finally","for",
    "fun","function","global","if","in","inherit","inline","interface","internal","land","lazy","let","lor",
    "lsl","lsr","lxor","match","member","mod","module","mutable","namespace","new","null","of","open","or",
    "override","private","public","rec","return","sig","static","struct","then","to",
    "true","try","type","upcast","use","val","void","when","while","with","yield",
    --reserved
    "atomic","break","checked","component","const","constraint","constructor",
    "continue","eager","fixed","fori","functor","include",
    "measure","method","mixin","object","parallel","params","process","protected","pure",
    "recursive","sealed","tailcall","trait","virtual","volatile",
    --used by BNFC
    "List", "Float", "Int"
    ]

-- | Avoid clashes with keywords.
sanitizeFSharp :: String -> String
sanitizeFSharp s
  | s `elem` reservedFSharp = s ++ "'"
  | otherwise = s

mkTuple :: [String] -> String
mkTuple [] = ""
mkTuple [x] = x
mkTuple xs = "(" ++ intercalate ", " xs ++ ")"

insertBar :: [String] -> [String]
insertBar = map ("| " ++)

fsharpTab:: String
fsharpTab = "    "
indent :: Int -> String -> String
indent n s =
    concat (replicate n fsharpTab) ++ s

mutualDefs :: [String] -> [String]
mutualDefs [] = []
mutualDefs (d:ds) = ("let rec" +++ d) : map ("and" +++) ds

-- | Escape @"@ and @\@.  TODO: escape unprintable characters!?
mkEsc :: String -> String
mkEsc s = "\"" ++ concatMap f s ++ "\""
  where
  f x = if x `elem` ['"','\\'] then "\\" ++ [x] else [x]

-- TODO: Replace keywords with fslex ones
-- | Keywords of @ocamllex@.
reservedFsLex :: [String]
reservedFsLex =
  [ "and"
  , "as"
  , "eof"
  , "let"
  , "parse"
  , "refill"
  , "rule"
  , "shortest"
  
  ,"letter" 
  ,"upper"
  ,"lower"
  ,"digit"
  ,"idchar"
  ,"universal"
  ,"rsyms"
  ]

-- | Heuristics to produce name for fslex token definition that
-- does not clash with the fslex keywords.
fsharpTokenName :: String -> String
fsharpTokenName x0
  | x `elem` reservedFsLex = x ++ "_"
  | otherwise                 = x
  where x = mapHead toLower x0

-- | map a CF nonterminal into a fsyacc symbol
nonterminal :: Cat -> String
nonterminal c = map spaceToUnderscore (fixType c)
    where spaceToUnderscore ' ' = '_'
          spaceToUnderscore x = x

prtFun :: Cat -> String
prtFun (ListCat c) = prtFun c ++ "ListBNFC"
prtFun c = "prt" ++ fixTypeUpper (normCat c)

showsFun :: Cat -> String
showsFun = showsFunQual id

showsFunQual :: (String -> String) -> Cat -> String
showsFunQual qual = loop where
  loop = \case
    ListCat c -> qual "showList" +++ loop c
    c         -> qual "show" ++ fixType (normCat c)