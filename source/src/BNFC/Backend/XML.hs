{-
    BNF Converter: XML generator
    Copyright (C) 2004  Author:  Aarne Ranta

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module BNFC.Backend.XML ---- (cf2DTD, cf2XML)
  where

import Prelude hiding ((<>))

import Data.Bifunctor ( second )
import Data.List      ( intercalate )

import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Utils
import BNFC.Backend.Base (Backend, mkfile)
import BNFC.Options (SharedOptions(..), pattern TargetHaskellGadt)
import BNFC.Backend.Haskell.CFtoTemplate ()
import BNFC.Backend.Haskell.HsOpts ( xmlFile, xmlFileM, absFileM )
import BNFC.Backend.Haskell.Utils  ( catToVar )
import qualified BNFC.Backend.Haskell.Utils as Haskell

type Coding = Bool ---- change to at least three values

makeXML :: SharedOptions -> Coding -> CF -> Backend
makeXML opts typ cf = do
  let name = lang opts
  mkfile (name ++ ".dtd") comment $ cf2DTD typ name cf
  let absmod = "XML" ++ name
  mkfile (xmlFile opts) Haskell.comment $ cf2XMLPrinter typ opts absmod cf

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

comment :: String -> String
comment x = unwords [ "<!--", x, "-->" ]

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
elemc cat fs = unlines $ element (prettyShow cat) (map snd fs) : [element f [] | (f,_) <- fs]

elemEmp :: String -> String
elemEmp t = elemAtt t "value" []

alts :: [String] -> String
alts ts = if null ts then "EMPTY" else parenth $ intercalate " | " ts

-- choose between these two encodings:

elemData b  = if b then elemDataConstr else elemDataNotyp
efunDef b   = if b then efunDefConstr else efunDefNotyp
endtagDef b = if b then endtagDefConstr else endtagDefNotyp

-- coding 0: ---- not finished
-- to show both types and constructors as tags;
-- lengthy, but validation guarantees type correctness
-- flag -xmlt
elemDataConstrs cf (cat,fcs) = elemc cat [(f,rhsCat cf f cs) | (f,cs) <- fcs]

efunDefConstrs :: String
efunDefConstrs = "elemFun i t x = [P.replicate (i+i) ' ' ++ tag t ++ \" \" ++ etag x]"

endtagDefConstrs :: String
endtagDefConstrs = "endtag _ c = tag (\"/\" ++ c)"

-- coding 1:
-- to show constructors as empty tags;
-- shorter than 0, but validation still guarantees type correctness
-- flag -xmlt
elemDataConstr cf (cat,fcs) = elemc cat [(f,rhsCat cf f cs) | (f,cs) <- fcs]
efunDefConstr = "elemFun i t x = [P.replicate (i+i) ' ' ++ tag t ++ \" \" ++ etag x]"
endtagDefConstr = "endtag _ c = tag (\"/\" ++ c)"

-- coding 2:
-- constructors as tags, no types.
-- clumsy DTD, but nice trees. Validation guarantees type correctness
-- flag -xml
elemDataNotyp cf (_,fcs) = unlines [element f [rhsCatNot cf cs] | (f,cs) <- fcs]
efunDefNotyp = "elemFun i _ x = [P.replicate (i+i) ' ' ++ tag x]"
endtagDefNotyp = "endtag f _ = tag (\"/\" ++ f)"


-- to show constructors as attributes;
-- nice, but validation does not guarantee type correctness.
-- Therefore rejected.
-- elemDataAttr cf (cat,fcs) = elemt cat (nub [rhsCat cf cs | (_,cs) <- fcs])
-- efunDefAttr =  "elemFun i t x = [replicate (i+i) ' ' ++ tag (t ++ \" name = \" ++ x)]"

rhsCat :: CF -> Fun -> [Cat] -> String
rhsCat cf fun cs = parenth (intercalate ", " (fun : map (render . symbCat cf) cs))
rhsCatNot cf cs = if null cs then "EMPTY" else intercalate ", " (map (render . symbCatNot cf) cs)

symbCat :: CF -> Cat -> Doc
symbCat cf c
  | isList c  = pretty (normCatOfList c) <> if isEmptyListCat cf c then "*" else "+"
  | otherwise = pretty c

symbCatNot :: CF -> Cat -> Doc
symbCatNot cf c
  | isList c  = funs (normCatOfList c) <> if isEmptyListCat cf c then "*" else "+"
  | otherwise = funs c
 where
   funs k =
     case lookup k (cf2data cf) of
       Just []  -> "EMPTY"
       Just fcs -> parens $ sep $ punctuate "|" $ map (text . fst) fcs
       _        -> parens $ pretty k

parenth s = "(" ++ s ++ ")"

-- derive an XML printer from a BNF grammar
cf2XMLPrinter :: Bool -> SharedOptions -> String -> CF -> String
cf2XMLPrinter typ opts absMod cf = unlines [
  "{-# LANGUAGE LambdaCase #-}",
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
  then "{-# LANGUAGE FlexibleInstances, GADTs #-}"
  else ""

prologue :: Bool -> SharedOptions -> String -> String
prologue b opts _ = unlines [
  "-- Pretty printing to XML",
  "",
  "module " ++ xmlFileM opts +++ "where",
  "",
  "import Prelude",
  "  ( Char, Double, Integer, String",
  "  , (.), ($), (+), (++)",
  "  )",
  "import qualified Prelude as P",
  "  ( Show(..), Int",
  "  , concat, concatMap, replicate, unlines",
  "  )",
  "",
  "import " ++ absFileM opts,
  "",
  "-- the top-level printing method",
  "printXML :: XPrint a => a -> String",
  "printXML = render . prt 0",
  "",
  "render :: [String] -> String",
  "render = P.unlines",
  "",
  "-- the printer class does the job",
  "class XPrint a where",
  "  prt :: P.Int -> a -> [String]",
  "  prtList :: P.Int -> [a] -> [String]",
  "  prtList = P.concatMap . prt",
  "",
  "instance XPrint a => XPrint [a] where",
  "  prt = prtList",
  "",
  "tag, etag :: String -> String",
  "tag t = \"<\" ++ t ++ \">\"",
  "etag t = \"<\" ++ t ++ \"/>\"",
  "",
  "elemTok, elemTokS :: P.Show a => P.Int -> String -> a -> [String]",
  "elemTok i t x = [P.replicate (i+i) ' ' ++ tag (t ++ \" value = \" ++ P.show x ++ \" /\")]",
  "elemTokS i t x = elemTok i t (P.show x)",
  "",
  "elemFun :: P.Int -> String -> String -> [String]",
  efunDef b,
  "",
  "endtag :: String -> String -> String",
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

identRule cf = ownPrintRule cf catIdent

ownPrintRule :: CF -> TokenCat -> String
ownPrintRule cf cat = unlines $
  [ "instance XPrint " ++ cat ++ " where"
  , "  prt i (" ++ cat ++ posn ++ ") = elemTok i" +++ "\"" ++ cat ++ "\"" +++ "x"
  ]
 where
   posn = if isPositionCat cf cat then " (_,x)" else " x"

rules :: CF -> String
rules cf = unlines $
  map (\ (s, xs) -> case_fun s (map (second toArgs) xs)) $ cf2data cf
 where
   toArgs args = names (map (catToVar ["prt"]) args) (0 :: Int)
   names [] _ = []
   names (x:xs) n
     | x `elem` xs = (x ++ show n) : names xs (n+1)
     | otherwise   = x             : names xs n

case_fun :: Cat -> [(String, [String])] -> String
case_fun cat xs = unlines $ concat
  [ [ "instance XPrint" +++ s +++ "where"
    , "  prt i'" +++ "= \\case"
    ]
  , (`map` xs) $ \ (c, xx) ->
    "   " ++ c +++ unwords xx +++ "-> P.concat $ " +++
    "elemFun i' \"" ++ s ++ "\" \"" ++ c ++ "\"" +++
    unwords [": prt (i'+1)" +++ x | x <- xx] +++ ":" +++
    "[[P.replicate (i'+i') ' ' ++ endtag \"" ++ c ++ "\" \"" ++ s ++ "\"]]"
  ]
  where
  s = prettyShow cat
