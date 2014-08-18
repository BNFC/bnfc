{-
    BNF Converter: Pretty-printer generator
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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

-- based on BNFC Haskell backend

module BNFC.Backend.OCaml.CFtoOCamlPrinter (cf2Printer) where

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.Haskell.CFtoTemplate
import Data.List (intersperse)
import Data.Char(toLower,isDigit)
import BNFC.Backend.OCaml.OCamlUtil

-- derive pretty-printer from a BNF grammar. AR 15/2/2002
cf2Printer :: String -> String -> CF -> String
cf2Printer name absMod cf = unlines [
  prologue name absMod,
  charRule cf,
  integerRule cf,
  doubleRule cf,
  stringRule cf,
  if hasIdent cf then identRule cf else "",
  unlines [ownPrintRule cf own | (own,_) <- tokenPragmas cf],
  rules cf
  ]


prologue :: String -> String -> String
prologue name absMod = unlines [
  "(* pretty-printer generated by the BNF converter *)\n",
  "open " ++ absMod,
  "open Printf",
  "",
  "(* We use string buffers for efficient string concatenation.",
  "   A document takes a buffer and an indentation, has side effects on the buffer",
  "   and returns a new indentation. The indentation argument indicates the level",
  "   of indentation to be used if a new line has to be started (because of what is",
  "   already in the buffer) *)",
  "type doc = Buffer.t -> int -> int",
  "",
  "let rec printTree (printer : int -> 'a -> doc) (tree : 'a) : string = ",
  "    let buffer_init_size = 16 (* you may want to change this *)",
  "    in let buffer = Buffer.create buffer_init_size",
  "    in ",
  "        let _ = printer 0 tree buffer 0 in (* discard return value *)",
  "        Buffer.contents buffer",
  "",
  "let indent_width = 4",
  "",
  "let indent (i: int) : string = ",
  "    let s = String.make (i+1) ' ' in",
  "    String.set s 0 '\\n';",
  "    s",
  "",
  "(* this render function is written for C-style languages, you may want to change it *)",
  "let render (s : string) : doc = fun buf i -> ",
  "    (* invariant: last char of the buffer is never whitespace *)",
  "    let n = Buffer.length buf in",
  "    let last = if n = 0 then None else Some (Buffer.nth buf (n-1)) in",
  "    let whitespace = match last with",
  "        None -> \"\" ",
  "      | Some '{' -> indent i",
  "      | Some '}' -> (match s with",
  "            \";\" -> \"\"",
  "          | _ -> indent i)",
  "      | Some ';' -> indent i",
  "      | (Some '[') |  (Some '(') -> \"\"",
  "      | Some _ -> (match s with",
  "            \",\" | \")\" | \"]\" -> \"\"",
  "           | _ -> \" \") in",
  "    let newindent = match s with",
  "        \"{\" -> i + indent_width",
  "      | \"}\" -> i - indent_width",
  "      | _ -> i in",
  "    Buffer.add_string buf whitespace;",
  "    Buffer.add_string buf s;",
  "    newindent",
  "",
  "let emptyDoc : doc = fun buf i -> i",
  "",
  "let concatD (ds : doc list) : doc = fun buf i -> ",
  "    List.fold_left (fun accIndent elemDoc -> elemDoc buf accIndent) (emptyDoc buf i) ds",
  "",
  "let parenth (d:doc) : doc = concatD [render \"(\"; d; render \")\"]",
  "",
  "let prPrec (i:int) (j:int) (d:doc) : doc = if j<i then parenth d else d",
  ""
  ]

charRule cf = unlines [
    "let rec prtChar (_:int) (c:char) : doc = render (\"'\" ^ Char.escaped c ^ \"'\")",
    ifList cf (Cat "Char"),
    ""
    ]

integerRule cf = unlines [
    "let rec prtInt (_:int) (i:int) : doc = render (string_of_int i)",
    ifList cf (Cat "Integer"),
    ""
    ]

doubleRule cf = unlines [
    "let rec prtFloat (_:int) (f:float) : doc = render (sprintf \"%f\" f)",
    ifList cf (Cat "Double"),
    ""
    ]

stringRule cf = unlines [
    "let rec prtString (_:int) (s:string) : doc = render (\"\\\"\" ^ String.escaped s ^ \"\\\"\")",
    ifList cf (Cat "String"),
    ""
    ]

identRule cf = ownPrintRule cf (Cat "Ident")

ownPrintRule cf own = unlines $ [
  "let rec" +++ prtFun own +++ "_ (" ++ show own ++ posn ++ ") : doc = render i",
  ifList cf own
  ]
 where
   posn = if isPositionCat cf own then " (_,i)" else " i"

-- copy and paste from CFtoTemplate

rules :: CF -> String
rules cf = unlines $ mutualDefs $
  map (\(s,xs) -> case_fun s (map toArgs xs) ++ ifList cf s) $ cf2data cf
 where
   toArgs (cons,args) = ((cons, names (map (checkRes . var) args) (0 :: Int)), ruleOf cons)
   names [] _ = []
   names (x:xs) n
     | elem x xs = (x ++ show n) : names xs (n+1)
     | otherwise = x             : names xs n
   var (ListCat c)  = var c ++ "s"
   var (Cat "Ident")   = "id"
   var (Cat "Integer") = "n"
   var (Cat "String")  = "str"
   var (Cat "Char")    = "c"
   var (Cat "Double")  = "d"
   var xs        = map toLower (show xs)
   checkRes s
        | elem s reservedOCaml = s ++ "'"
        | otherwise              = s
   ruleOf s = maybe undefined id $ lookupRule s (rulesOfCF cf)

--- case_fun :: Cat -> [(Constructor,Rule)] -> String
case_fun cat xs = unlines [
--  "instance Print" +++ cat +++ "where",
  prtFun cat +++"(i:int)" +++ "(e:" ++ fixType cat ++ ") : doc = match e with",
  unlines $ insertBar $ map (\ ((c,xx),r) ->
    "   " ++ c +++ mkTuple xx +++ "->" +++
    "prPrec i" +++ show (precCat (fst r)) +++ mkRhs xx (snd r)) xs
  ]

ifList cf cat = mkListRule $ nil cat ++ one cat ++ cons cat where
  nil cat  = ["    []    -> " ++ mkRhs [] its |
                            Rule f c its <- rulesOfCF cf, isNilFun f , normCatOfList c == cat]
  one cat  = ["  | [x]   -> " ++ mkRhs ["x"] its |
                            Rule f c its <- rulesOfCF cf, isOneFun f , normCatOfList c == cat]
  cons cat = ["  | x::xs -> " ++ mkRhs ["x","xs"] its |
                            Rule f c its <- rulesOfCF cf, isConsFun f , normCatOfList c == cat]
  mkListRule [] = ""
  mkListRule rs = unlines $ ("and prt" ++ fixTypeUpper cat ++ "ListBNFC" +++ "_ es : doc = match es with"):rs


mkRhs args its =
  "(concatD [" ++ unwords (intersperse ";" (mk args its)) ++ "])"
 where
  mk args (Left InternalCat : items)      = mk args items
  mk (arg:args) (Left c : items)  = (prt c +++ arg)        : mk args items
  mk args       (Right s : items) = ("render " ++ show s) : mk args items
  mk _ _ = []
  prt c = prtFun c +++ show (precCat c)

prtFun :: Cat -> String
prtFun (ListCat c) = prtFun c ++ "ListBNFC"
prtFun c = "prt" ++ fixTypeUpper (normCat c)

