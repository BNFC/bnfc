{-
    BNF Converter: Non-pretty-printer generator (no "deriving Show" in OCaml...)
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

-- there is no "deriving Show" in OCaml, although there are solutions based
-- on camlp4. Here we generate our own "show module".


module CFtoOCamlShow (cf2show) where

import CF
import Utils
import CFtoTemplate
import Data.List (intersperse)
import Data.Char(toLower,isDigit)
import OCamlUtil

cf2show :: String -> String -> CF -> String
cf2show name absMod cf = unlines [
  prologue name absMod,
  integerRule cf,
  doubleRule cf,
  if hasIdent cf then identRule cf else "",
  unlines [ownPrintRule cf own | (own,_) <- tokenPragmas cf],
  rules cf
  ]


prologue :: String -> String -> String
prologue name absMod = unlines [
  "(* show functions generated by the BNF converter *)\n",
  "open " ++ absMod,
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

integerRule cf = "let showInt (i:int) : showable = s2s (string_of_int i)"

doubleRule cf = "let showFloat (f:float) : showable = s2s (string_of_float f)"


identRule cf = ownPrintRule cf "Ident"

ownPrintRule cf own = unlines $ [
  "let rec" +++ showsFun own +++ "(" ++ own ++ posn ++ ") : showable = s2s \""
  ++ own ++ " \" >> showString i"
  ]
 where
   posn = if isPositionCat cf own then " (_,i)" else " i"

-- copy and paste from CFtoTemplate

rules :: CF -> String
rules cf = unlines $ mutualDefs $
  map (\(s,xs) -> case_fun s (map toArgs xs)) $ cf2data cf -- ++ ifList cf s
 where
   toArgs (cons,args) = ((cons, names (map (checkRes . var) args) (0 :: Int)),
                         ruleOf cons)
   names [] _ = []
   names (x:xs) n
     | elem x xs = (x ++ show n) : names xs (n+1)
     | otherwise = x             : names xs n
   var ('[':xs)  = var (init xs) ++ "s"
   var "Ident"   = "id"
   var "Integer" = "n"
   var "String"  = "str"
   var "Char"    = "c"
   var "Double"  = "d"
   var xs        = map toLower xs
   checkRes s
        | elem s reservedOCaml = s ++ "'"
        | otherwise              = s
   ruleOf s = maybe undefined id $ lookup s (rulesOfCF cf)

--- case_fun :: Cat -> [(Constructor,Rule)] -> String
case_fun cat xs = unlines [
--  "instance Print" +++ cat +++ "where",
  showsFun cat +++ "(e:" ++ fixType cat ++ ") : showable = match e with",
  unlines $ insertBar $ map (\ ((c,xx),r) ->
    "   " ++ c +++ mkTuple xx +++ "->" +++
    "s2s" +++ show c +++
    case mkRhs xx (snd r) of {[] -> []; str -> ">> c2s ' ' >> " ++ str}
    )
    xs
  ]


mkRhs args its =
  case unwords (intersperse " >> s2s \", \" >> " (mk args its)) of
    [] -> ""
    str -> "c2s '(' >> " ++ str ++ " >> c2s ')'"
 where
  mk args (Left "#" : items)      = mk args items
  mk (arg:args) (Left c : items)  = (showsFun c +++ arg)        : mk args items
  mk args       (Right s : items) = mk args items
  mk _ _ = []
  prt c = showsFun c +++ show (precCat c)

showsFun :: Cat -> String
showsFun c = case c of
    '[':xs -> case break (== ']') xs of
        (t,"]") -> "showList" +++ showsFun t -- showFun t ++ "List"
        _ -> c -- should not occur (this means an invariant of the type Cat is broken)
    _ -> if precCat c > 0 -- precedence-level cats are not in abstract syntax
            then "show" ++ (fixTypeUpper $ reverse (dropWhile isDigit (reverse c)))
            else "show" ++ (fixTypeUpper c)
