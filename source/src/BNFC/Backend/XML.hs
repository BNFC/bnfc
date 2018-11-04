{-
    BNF Converter: XML generator
    Copyright (C) 2004  Author:  Aarne Ranta

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

module BNFC.Backend.XML ---- (cf2DTD, cf2XML)
  where

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.Base
import BNFC.Options hiding (Backend)
import BNFC.Backend.Haskell.CFtoTemplate ()
import BNFC.Backend.Haskell.HsOpts (xmlFile, xmlFileM, absFileM)
import Data.List (intersperse, intercalate)
import Data.Char(toLower)
import Data.Maybe (fromJust)

type Coding = Bool ---- change to at least three values

makeXML :: SharedOptions -> Coding -> CF -> Backend
makeXML opts typ cf = do
  let name = lang opts
  mkfile (name ++ ".dtd") $ cf2DTD typ name cf
  let absmod = "XML" ++ name
  mkfile (xmlFile opts) $ cf2XMLPrinter typ opts absmod cf

-- derive a DTD from a BNF grammar. AR 21/8/2004
cf2DTD :: Coding -> String -> CF -> String
cf2DTD typ name cf = unlines [
  tag "?xml version=\"1.0\" standalone=\"yes\"?",
  "<!DOCTYPE " ++ name ++ " [",
  elemEmp "Integer",
  elemEmp "Double",
  elemEmp "String",
  if hasIdent cf then elemEmp "Ident" else "",
  unlines [elemEmp own | own <- tokenNames cf],
  unlines (map (elemData typ cf) (cf2data cf)),
  "]>"
  ]

-- | >>> tag "test"
-- "<test>"
tag :: String -> String
tag s = "<" ++ s ++ ">"

element :: String -> [String] -> String
element t ts =
  tag ("!ELEMENT " ++ t ++ " " ++ alts ts)

attlist t a =
  tag ("!ATTLIST " ++ t ++ " " ++ a ++ " CDATA #REQUIRED")
elemAtt t a ts = element t ts ++++ attlist t a
elemt t = elemAtt t "name"

elemc :: Cat -> [(Fun, String)] -> String
elemc cat fs = unlines $ element (show cat) (map snd fs) : [element f [] | (f,_) <- fs]

elemEmp :: String -> String
elemEmp t = elemAtt t "value" []

alts :: [String] -> String
alts ts =
  if null ts then "EMPTY" else parenth (unwords (intersperse "|" ts))


-- choose between these two encodings:

elemData b  = if b then elemDataConstr else elemDataNotyp
efunDef b   = if b then efunDefConstr else efunDefNotyp
endtagDef b = if b then endtagDefConstr else endtagDefNotyp

-- coding 0: ---- not finished
-- to show both types and constructors as tags;
-- lengthy, but validation guarantees type correctness
-- flag -xmlt
elemDataConstrs cf (cat,fcs) = elemc cat [(f,rhsCat cf f cs) | (f,cs) <- fcs]
efunDefConstrs = "elemFun i t x = [replicate (i+i) ' ' ++ tag t ++ \" \" ++ etag x]"
endtagDefConstrs = "endtag f c = tag (\"/\" ++ c)"

-- coding 1:
-- to show constructors as empty tags;
-- shorter than 0, but validation still guarantees type correctness
-- flag -xmlt
elemDataConstr cf (cat,fcs) = elemc cat [(f,rhsCat cf f cs) | (f,cs) <- fcs]
efunDefConstr = "elemFun i t x = [replicate (i+i) ' ' ++ tag t ++ \" \" ++ etag x]"
endtagDefConstr = "endtag f c = tag (\"/\" ++ c)"

-- coding 2:
-- constructors as tags, no types.
-- clumsy DTD, but nice trees. Validation guarantees type correctness
-- flag -xml
elemDataNotyp cf (_,fcs) = unlines [element f [rhsCatNot cf cs] | (f,cs) <- fcs]
efunDefNotyp = "elemFun i t x = [replicate (i+i) ' ' ++ tag x]"
endtagDefNotyp = "endtag f c = tag (\"/\" ++ f)"


-- to show constructors as attributes;
-- nice, but validation does not guarantee type correctness.
-- Therefore rejected.
-- elemDataAttr cf (cat,fcs) = elemt cat (nub [rhsCat cf cs | (_,cs) <- fcs])
-- efunDefAttr =  "elemFun i t x = [replicate (i+i) ' ' ++ tag (t ++ \" name = \" ++ x)]"

rhsCat :: CF -> Fun -> [Cat] -> String
rhsCat cf fun cs = parenth (intercalate ", " (fun:map (symbCat cf) cs))
rhsCatNot cf cs = if null cs then "EMPTY" else intercalate", " (map (symbCatNot cf) cs)

symbCat cf c
  | isList c  = show (normCatOfList c) ++ if isEmptyListCat cf c then "*" else "+"
  | otherwise = show c

symbCatNot cf c
  | isList c  = funs (normCatOfList c) ++ if isEmptyListCat cf c then "*" else "+"
  | otherwise = funs c
 where
   funs k = case lookup k (cf2data cf) of
     Just []  -> "EMPTY"
     Just fcs -> parenth $ unwords $ intersperse "|" $ map fst fcs
     _ -> parenth (show k) ----

parenth s = "(" ++ s ++ ")"

-- derive an XML printer from a BNF grammar
cf2XMLPrinter :: Bool -> SharedOptions -> String -> CF -> String
cf2XMLPrinter typ opts absMod cf = unlines [
  pragmas opts,
  prologue typ opts absMod,
  integerRule cf,
  doubleRule cf,
  stringRule cf,
  if hasIdent cf then identRule cf else "",
  unlines [ownPrintRule cf own | (own,_) <- tokenPragmas cf],
  rules cf
  ]

pragmas :: SharedOptions -> String
pragmas opts =
  if target opts == TargetHaskellGadt
  then "{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GADTs #-}"
  else ""

prologue :: Bool -> SharedOptions -> String -> String
prologue b opts _ = unlines [
  "module " ++ xmlFileM opts +++ "where\n",
  "-- pretty-printer generated by the BNF converter\n",
  "import " ++ absFileM opts,
  "import Data.Char",
  "",
  "-- the top-level printing method",
  "printXML :: XPrint a => a -> String",
  "printXML = render . prt 0",
  "",
  "render = unlines",
  "",
  "-- the printer class does the job",
  "class XPrint a where",
  "  prt :: Int -> a -> [String]",
  "  prtList :: Int -> [a] -> [String]",
  "  prtList i = concat . map (prt i)",
  "",
  "instance XPrint a => XPrint [a] where",
  "  prt = prtList",
  "",
  "tag t = \"<\" ++ t ++ \">\"",
  "etag t = \"<\" ++ t ++ \"/>\"",
  "elemTok i t x = [replicate (i+i) ' ' ++ tag (t ++ \" value = \" ++ show x ++ \" /\")]",
  "elemTokS i t x = elemTok i t (show x)",
  efunDef b,
  endtagDef b,
  ""
  ]

integerRule cf = showsPrintRule cf "Integer"
doubleRule cf = showsPrintRule cf "Double"
stringRule cf = showsPrintRule cf "Char" ++++ "  prtList i xs = elemTok i \"String\" xs"

showsPrintRule _ t = unlines [
  "instance XPrint " ++ t ++ " where",
  "  prt i x = elemTokS i" +++ "\"" ++ t ++ "\"" +++ "x"
  ]

identRule cf = ownPrintRule cf (Cat "Ident")

ownPrintRule cf cat = unlines [
  "instance XPrint " ++ show cat ++ " where",
  "  prt i (" ++ show cat ++ posn ++ ") = elemTok i" +++ "\"" ++ show cat ++ "\"" +++ "x"
  ]
 where
   posn = if isPositionCat cf cat then " (_,x)" else " x"

rules :: CF -> String
rules cf = unlines $
  map (\(s,xs) -> case_fun s (map toArgs xs)) $ cf2data cf
 where
   toArgs (cons,args) = ((cons, names (map (checkRes . var) args) (0 :: Int)), ruleOf cons)
   names [] _ = []
   names (x:xs) n
     | x `elem` xs = (x ++ show n) : names xs (n+1)
     | otherwise = x             : names xs n
   var (ListCat c)  = var c ++ "s"
   var (Cat "Ident")   = "id"
   var (Cat "Integer") = "n"
   var (Cat "String")  = "str"
   var (Cat "Char")    = "c"
   var (Cat "Double")  = "d"
   var cat            = map toLower (show cat)
   checkRes s
        | s `elem` reservedHaskell = s ++ "'"
        | otherwise              = s
   reservedHaskell = ["case","class","data","default","deriving","do","else","if",
                          "import","in","infix","infixl","infixr","instance","let","module",
                          "newtype","of","then","type","where","as","qualified","hiding"]
   ruleOf s = fromJust $ lookupRule s (cfgRules cf)

--- case_fun :: Cat -> [(Constructor,Rule)] -> String
case_fun cat xs = unlines [
  "instance XPrint" +++ show cat +++ "where",
  "  prt i" +++ "e = case e of",
  unlines $ map (\ ((c,xx),_) ->
    "   " ++ c +++ unwords xx +++ "-> concat $ " +++
    "elemFun i \"" ++ show cat ++ "\" \"" ++ c ++ "\"" +++
    unwords [": prt (i+1)" +++ x | x <- xx] +++ ":" +++
    "[[replicate (i+i) ' ' ++ endtag \"" ++ c ++ "\" \"" ++ show cat ++ "\"]]"
    ) xs
  ]
