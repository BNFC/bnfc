{-
    BNF Converter: Non-pretty-printer generator (no "deriving Show" in OCaml...)
    Copyright (C) 2005  Author:  Kristofer Johannisson

-}

-- there is no "deriving Show" in OCaml, although there are solutions based
-- on camlp4. Here we generate our own "show module".

{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.OCaml.CFtoOCamlShow (cf2show, showsFunQual) where

import Data.Char(toLower)
import Data.List (intersperse)
import Data.Maybe (fromJust)

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.OCaml.OCamlUtil

cf2show :: String -> ModuleName -> CF -> String
cf2show _name absMod cf = unlines
  [ prologue
  , integerRule
  , doubleRule
  , if hasIdent cf then identRule absMod cf else ""
  , unlines [ ownPrintRule absMod cf own | (own,_) <- tokenPragmas cf ]
  , rules absMod cf
  ]


prologue :: String
prologue = unlines [
  "(* show functions *)",
  "",
  "(* use string buffers for efficient string concatenations *)",
  "type showable = Buffer.t -> unit",
  "",
  "let show (s : showable) : string = ",
  "    let init_size = 16 in (* you may want to adjust this *)",
  "    let b = Buffer.create init_size in",
  "    s b;",
  "    Buffer.contents b",
  "    ",
  "let emptyS : showable = fun buf -> ()",
  "",
  "let c2s (c:char) : showable = fun buf -> Buffer.add_char buf c",
  "let s2s (s:string) : showable = fun buf -> Buffer.add_string buf s",
  "",
  "let ( >> ) (s1 : showable) (s2 : showable) : showable = fun buf -> s1 buf; s2 buf",
  "",
  "let showChar (c:char) : showable = fun buf -> ",
  "    Buffer.add_string buf (\"'\" ^ Char.escaped c ^ \"'\")",
  "",
  "let showString (s:string) : showable = fun buf -> ",
  "    Buffer.add_string buf (\"\\\"\" ^ String.escaped s ^ \"\\\"\")",
  "",
  "let showList (showFun : 'a -> showable) (xs : 'a list) : showable = fun buf -> ",
  "    let rec f ys = match ys with",
  "        [] -> ()",
  "      | [y] -> showFun y buf",
  "      | y::ys -> showFun y buf; Buffer.add_string buf \"; \"; f ys ",
  "    in",
  "        Buffer.add_char buf '[';",
  "        f xs;",
  "        Buffer.add_char buf ']'",
  ""
  ]

integerRule :: String
integerRule = "let showInt (i:int) : showable = s2s (string_of_int i)"

doubleRule :: String
doubleRule = "let showFloat (f:float) : showable = s2s (string_of_float f)"

identRule :: ModuleName -> CF -> String
identRule absMod cf = ownPrintRule absMod cf catIdent

ownPrintRule :: ModuleName -> CF -> TokenCat -> String
ownPrintRule absMod cf own =
  "let rec" +++ showsFun (TokenCat own) +++ "(" ++ absMod ++ "." ++ own ++ posn ++ ") : showable = s2s \""
  ++ own ++ " \" >> showString i"
 where
   posn = if isPositionCat cf own then " (_,i)" else " i"

-- copy and paste from CFtoTemplate

rules :: ModuleName -> CF -> String
rules absMod cf = unlines $ mutualDefs $
  map (\ (s, xs) -> case_fun absMod s $ map toArgs xs) $ cf2data cf
  where
   toArgs (cons,args) = ((cons, names (map (checkRes . var) args) (0 :: Int)),
                         ruleOf cons)
   names [] _ = []
   names (x:xs) n
     | elem x xs = (x ++ show n) : names xs (n+1)
     | otherwise = x             : names xs n
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
   ruleOf s = fromJust $ lookupRule (noPosition s) (cfgRules cf)

-- case_fun :: Cat -> [(Constructor,Rule)] -> String
case_fun :: String
         -> Cat -> [((String, [String]), (a, [Either Cat t]))] -> String
case_fun absMod cat xs = unlines [
  showsFun cat +++ "(e : " ++ fixTypeQual absMod cat ++ ") : showable = match e with",
  unlines $ insertBar $ map (\ ((c,xx),r) ->
    "   " ++ absMod ++ "." ++ c +++ mkTuple xx +++ "->" +++
    "s2s" +++ show c +++
    case mkRhs xx (snd r) of {[] -> []; str -> ">> c2s ' ' >> " ++ str}
    )
    xs
  ]

mkRhs :: [String] -> [Either Cat t] -> String
mkRhs args its =
  case unwords (intersperse " >> s2s \", \" >> " (mk args its)) of
    [] -> ""
    str -> "c2s '(' >> " ++ str ++ " >> c2s ')'"
 where
  mk (arg:args) (Left c : items)  = (showsFun c +++ arg)        : mk args items
  mk args       (Right _ : items) = mk args items
  mk _ _ = []

showsFun :: Cat -> String
showsFun = showsFunQual id

showsFunQual :: (String -> String) -> Cat -> String
showsFunQual qual = loop where
  loop = \case
    ListCat c -> qual "showList" +++ loop c
    c         -> qual "show" ++ (fixTypeUpper $ normCat c)
