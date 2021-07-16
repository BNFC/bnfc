{-
    BNF Converter: Template Generator
    Copyright (C) 2005  Author:  Kristofer Johannisson

-}

-- based on BNFC Haskell backend


module BNFC.Backend.OCaml.CFtoOCamlTemplate (
                    cf2Template
                    ) where

import Data.Char
import BNFC.CF
import BNFC.Backend.OCaml.OCamlUtil


type ModuleName = String
type Constructor = String

cf2Template :: ModuleName -> ModuleName  -> CF -> String
cf2Template skelName absName cf = unlines
  [
  "module "++ skelName ++ " = struct\n",
  "open " ++ absName ++ "\n",
  "type result = string\n",
  "let failure x = failwith \"Undefined case.\" (* x discarded *)\n",
  unlines $ mutualDefs $ map (\(s,xs) -> case_fun s (toArgs xs)) $ specialData cf ++ cf2data cf,
  "end"
  ]
 where toArgs               [] = []
       toArgs ((cons,args):xs)
           = (cons ++ " " ++  (mkTuple $ names (map (checkRes . var) args) (0 :: Int))) : toArgs xs
       names :: [String] -> Int -> [String]
       names [] _ = []
       names (x:xs) n
        | elem x xs = (x ++ show n) : names xs (n+1)
        | otherwise = x : names xs n
       var (ListCat c)      = var c ++ "s"
       var (Cat "Ident")    = "id"
       var (Cat "Integer")  = "n"
       var (Cat "String")   = "str"
       var (Cat "Char")     = "c"
       var (Cat "Double")   = "d"
       var cat              = map toLower (catToStr cat)
       checkRes s
        | elem s reservedOCaml = s ++ "'"
        | otherwise              = s


case_fun :: Cat -> [Constructor] -> String
case_fun cat xs =
 unlines $
         ["trans" ++ catToStr cat ++ " (x : " ++ fixType cat ++ ") : result = match x with",
          unlines $ insertBar $ map (\s -> s ++ " -> " ++ "failure x") xs]
