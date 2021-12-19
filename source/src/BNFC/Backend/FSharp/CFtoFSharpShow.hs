{-
    TODO: Check if printing fot native fsharp objects like choice types or algebraic types can handle this (it would allow to remove most of the code here)
    BNF Converter: Non-pretty-printer generator 
    Copyright (C) 2021  Author:  Grzegorz Dziadkiewicz

-}


{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.FSharp.CFtoFSharpShow (cf2show, showsFunQual) where

import Data.Char(toLower)
import Data.List (intersperse)
import Data.Maybe (fromJust)

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.FSharp.FSharpUtil

cf2show :: String -> ModuleName -> CF -> String
cf2show name absMod cf = unlines
  [ prologue name absMod
  , integerRule
  , doubleRule
  , if hasIdent cf then identRule absMod cf else ""
  , unlines [ ownPrintRule absMod cf own | (own,_) <- tokenPragmas cf, own /= "Integer" && own /= "Double"]
  , rules absMod cf
  ]

prologue :: String -> String -> String
prologue name absMod = unlines [
  "// show functions",
  "module " ++ name,
  "",
  "open System.Text",
  "open " ++ absMod,
  "",
  "type Showable = StringBuilder -> unit",
  "",
  "let show (s : Showable) : string = ",
  "    let initSize = 16",
  "    let b = StringBuilder initSize",
  "    s b",
  "    b.ToString()",
  "",
  "let emptyS : Showable = ignore",
  "",
  "let c2s (c:char) : Showable = fun buf -> buf.Append c |> ignore",
  "let s2s (s:string) : Showable = fun buf -> buf.Append s |> ignore",
  "",
  "let ( >> ) (s1:Showable) (s2:Showable) : Showable = fun buf ->",
  "    s1 buf",
  "    s2 buf",
  "",
  "let " ++ showsFun (TokenCat "Char") ++ " (c:char) : Showable = fun buf -> ",
  "    buf.Append (\"'\" + string c + \"'\") |> ignore",
  "",
  "let " ++ showsFun (TokenCat "String") ++ " (s:string) : Showable = fun buf -> ",
  "    buf.Append (\"\\\"\" + s + \"\\\"\") |> ignore",
  "",
  "let showList (showFun : 'a -> Showable) (xs : 'a list) : Showable = fun buf -> ",
  "    let rec f ys =",
  "        match ys with",
  "        | [] -> ()",
  "        | [y] -> showFun y buf",
  "        | y::ys ->",
  "            showFun y buf",
  "            buf.Append \"; \" |> ignore",
  "            f ys ",
  "    buf.Append '[' |> ignore",
  "    f xs;",
  "    buf.Append ']' |> ignore"
  ]

integerRule = "let showint (i:int) : Showable = i |> string |> s2s"

doubleRule = "let showfloat (f:float) : Showable = f |> string |> s2s"

identRule absMod cf = ownPrintRule absMod cf catIdent

ownPrintRule :: ModuleName -> CF -> TokenCat -> String
ownPrintRule absMod cf own =
  "let rec" +++ showsFun tokenCat +++ "(" ++ fixTypeQual absMod tokenCat ++ posn ++ ") : Showable = s2s \""
  ++ own ++ " \" >> showstring i"
 where
   tokenCat = TokenCat own
   posn = if isPositionCat cf own then " (_,i)" else " i"

-- copy and paste from CFtoTemplate

rules :: ModuleName -> CF -> String
rules absMod cf = unlines $ mutualDefs $
  map (\ (s, xs) -> case_fun absMod s $ map toArgs xs) $ cf2data cf
  where
   toArgs (cons,args) = ((cons, names (map (sanitizeFSharp . var) args) (0 :: Int)),
                         ruleOf cons)
   names [] _ = []
   names (x:xs) n
     | x `elem` xs = (x ++ show n) : names xs (n+1)
     | otherwise = x             : names xs n
   var (ListCat c)      = var c ++ "s"
   var (Cat "Ident")    = "id"
   var (Cat "Integer")  = "n"
   var (Cat "String")   = "str"
   var (Cat "Char")     = "c"
   var (Cat "Double")   = "d"
   var cat              = map toLower (catToStr cat)
   ruleOf s = fromJust $ lookupRule (noPosition s) (cfgRules cf)

-- case_fun :: Cat -> [(Constructor,Rule)] -> String
case_fun absMod cat xs = unlines [
  showsFun cat +++ "(e : " ++ fixType cat ++ ") : Showable =",
  indent 4 "match e with",
  unlines $ map (indent 4) $ insertBar $ map f xs
 ]
 where
   f ((c,xx),r) =
    "   " ++ absMod ++ "." ++ c +++ mkTuple xx +++ "->" +++
    "s2s" +++ show c +++
    case mkRhs xx (snd r) of
      [] -> []
      str -> ">> c2s ' ' >> " ++ str

mkRhs args its =
  case unwords (intersperse " >> s2s \", \" >> " (mk args its)) of
    [] -> ""
    str -> "c2s '(' >> " ++ str ++ " >> c2s ')'"
 where
  mk (arg:args) (Left c : items)  = (showsFun c +++ arg)        : mk args items
  mk args       (Right _ : items) = mk args items
  mk _ _ = []


