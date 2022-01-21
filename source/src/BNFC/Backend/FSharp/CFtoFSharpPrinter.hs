{-
    BNF Converter: Pretty-printer generator
    Copyright (C) 2021  Author:  Grzegorz Dziadkiewicz

-}

-- based on BNFC OCaml backend


{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.FSharp.CFtoFSharpPrinter (cf2Printer, prtFun) where

import Prelude hiding ((<>))

import Data.Char(toLower)
import Data.List (intersperse, sortBy)
import Data.Maybe (fromJust)

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.FSharp.FSharpUtil
import BNFC.PrettyPrint
import BNFC.Backend.Haskell.CFtoPrinter (compareRules)


-- derive pretty-printer from a BNF grammar. AR 15/2/2002
cf2Printer :: String -> ModuleName -> CF -> String
cf2Printer name absMod cf = unlines [
  prologue name absMod,
  charRule cf,
  integerRule cf,
  doubleRule cf,
  stringRule cf,
  if hasIdent cf then identRule absMod cf else "",
  unlines [ownPrintRule absMod cf own | (own,_) <- tokenPragmas cf, own /= "Integer" && own /= "Double"],
  rules absMod cf
  ]

prologue :: String -> String -> String
prologue name absMod = unlines [
  "// pretty-printer",
  "module " ++ name,
  "",
  "open System",
  "open System.Text",
  "open " ++ absMod,
  "open Printf",
  "",
  "type Doc = StringBuilder -> int -> int",
  "",
  "let rec printTree (printer : int -> 'a -> Doc) (tree : 'a) : string = ",
  "    let initSize = 16",
  "    let buffer = StringBuilder initSize",
  "    printer 0 tree buffer 0 |> ignore",
  "    buffer.ToString()",
  "",
  "let indentWidth = 4",
  "",
  "let indent (i: int) : string = \"\\n\" + String.replicate i \" \"",
  "let append (s:string) (sb:StringBuilder) = sb.Append s",
  "",
  "// this render function is written for C-style languages, you may want to change it",
  "let render (s : string) : Doc = fun buf i -> ",
  "    let n = buf.Length",
  "    let last = if n = 0 then None else Some (buf.Chars (n-1))",
  "    let newindent =",
  "        match s with",
  "        | \"{\" -> i + indentWidth",
  "        | \"}\" -> i - indentWidth",
  "        | _ -> i",
  "    let whitespace =",
  "        match last with",
  "        | None -> \"\" ",
  "        | Some '}' ->",
  "            match s with",
  "            | \";\" -> \"\"",
  "            | _ -> indent newindent",
  "        | Some '{' | Some ';' when s = \"}\" -> indent newindent",
  "        | Some '{' | Some ';' -> indent i",
  "        | Some '[' |  Some '(' -> \"\"",
  "        | Some c when Char.IsWhiteSpace c -> \"\"",
  "        | Some _ ->",
  "           match s with",
  "           |  \";\" | \",\" | \")\" | \"]\" -> \"\"",
  "           | \"{\" -> indent i",
  "           | \"}\" -> indent newindent",
  "           | _ when String.IsNullOrWhiteSpace s -> \"\"",
  "           | _ -> \" \"",
  "    buf |> append whitespace |> append s |> ignore",
  "    newindent",
  "",
  "let emptyDoc : Doc = fun _ i -> i",
  "",
  "let concatD (ds : Doc list) : Doc = fun buf i -> ",
  "    List.fold (fun accIndent elemDoc -> elemDoc buf accIndent) (emptyDoc buf i) ds",
  "",
  "let parenth (d:Doc) : Doc = concatD [render \"(\"; d; render \")\"]",
  "",
  "let prPrec (i:int) (j:int) (d:Doc) : Doc = if j<i then parenth d else d",
  ""
  ]

charRule cf = unlines [
    "let rec prtChar (_:int) (c:char) : Doc = render (\"'\" + string c + \"'\")",
    ifList cf (TokenCat catChar),
    ""
    ]

integerRule cf = unlines [
    "let rec prtInt (_:int) (i:int) : Doc = render (string i)",
    ifList cf (TokenCat catInteger),
    ""
    ]

doubleRule cf = unlines [
    "let rec prtFloat (_:int) (f:float) : Doc = render (f.ToString().ToLower())",
    ifList cf (TokenCat catDouble),
    ""
    ]

stringRule cf = unlines [
    "let rec prtString (_:int) (s:string) : Doc = render (\"\\\"\" + s + \"\\\"\")",
    ifList cf (TokenCat catString),
    ""
    ]

identRule absMod cf = ownPrintRule absMod cf catIdent

ownPrintRule :: ModuleName -> CF -> TokenCat -> String
ownPrintRule absMod cf own = unlines $ [
  "let rec" +++ prtFun (TokenCat own) +++ "_ (" ++ absMod ++ "." ++ fixType (TokenCat own) ++ posn ++ ") : Doc = render i",
  ifList cf (TokenCat own)
  ]
 where
   posn = if isPositionCat cf own then " (_,i)" else " i"

-- copy and paste from CFtoTemplate

rules :: ModuleName -> CF -> String
rules absMod cf = unlines $ mutualDefs $
  map (\(s,xs) -> case_fun absMod s (map toArgs xs) ++ ifList cf s) $ cf2data cf
 where
   reserved = "i":"e":reservedFSharp
   toArgs (cons,args) = ((cons, mkNames reserved LowerCase (map var args)), ruleOf cons)
   var (ListCat c)  = var c ++ "s"
   var (Cat "Ident")   = "id"
   var (Cat "Integer") = "n"
   var (Cat "String")  = "str"
   var (Cat "Char")    = "c"
   var (Cat "Double")  = "d"
   var xs   = map toLower (catToStr xs)
   ruleOf s = fromJust $ lookupRule (noPosition s) (cfgRules cf)

--- case_fun :: Cat -> [(Constructor,Rule)] -> String
case_fun absMod cat xs = unlines [
--  "instance Print" +++ cat +++ "where",
  prtFun cat +++"(i:int)" +++ "(e : " ++ fixTypeQual absMod cat ++ ") : Doc =",
  "    match e with",
  unlines $ map (indent 1) $ insertBar $ map g xs
  ]
  where
    g ((c,xx),r) =
      "   " ++ absMod ++ "." ++ c +++ mkTuple xx +++ "->" +++
      "prPrec i" +++ show (precCat (fst r)) +++ mkRhs xx (snd r)

-- ifList cf cat = mkListRule $ nil cat ++ one cat ++ cons cat where
--   nil cat  = ["    []    -> " ++ mkRhs [] its |
--                             Rule f c its <- rulesOfCF cf, isNilFun f , normCatOfList c == cat]
--   one cat  = ["  | [x]   -> " ++ mkRhs ["x"] its |
--                             Rule f c its <- rulesOfCF cf, isOneFun f , normCatOfList c == cat]
--   cons cat = ["  | x::xs -> " ++ mkRhs ["x","xs"] its |
--                             Rule f c its <- rulesOfCF cf, isConsFun f , normCatOfList c == cat]
--   mkListRule [] = ""
--   mkListRule rs = unlines $ ("and prt" ++ fixTypeUpper cat ++ "ListBNFC" +++ "_ es : doc = match es with"):rs

ifList :: CF -> Cat -> String
ifList cf cat = render $
  case cases of
    []         -> empty
    xs -> vcat
        [ "and prt" <> text (fixTypeUpper cat)  <> "ListBNFC i es : Doc ="
        , nest 4 "match (i, es) with"
        , nest 4 $ vcat (map ("|" <+>) xs)
        ]
  where
    rules = sortBy compareRules $ rulesForNormalizedCat cf (ListCat cat)
    cases = [ d | r <- rules, let d = mkPrtListCase r, not (isEmpty d) ]


-- | Pattern match on the list constructor and the coercion level
--
-- >>> mkPrtListCase (npRule "[]" (ListCat (Cat "Foo")) [] Parsable)
-- (_,[]) -> (concatD [])
--
-- >>> mkPrtListCase (npRule "(:[])" (ListCat (Cat "Foo")) [Left (Cat "Foo")] Parsable)
-- (_,[x]) -> (concatD [prtFoo 0 x])
--
-- >>> mkPrtListCase (npRule "(:)" (ListCat (Cat "Foo")) [Left (Cat "Foo"), Left (ListCat (Cat "Foo"))] Parsable)
-- (_,x::xs) -> (concatD [prtFoo 0 x ; prtFooListBNFC 0 xs])
--
-- >>> mkPrtListCase (npRule "[]" (ListCat (CoercCat "Foo" 2)) [] Parsable)
-- (2,[]) -> (concatD [])
--
-- >>> mkPrtListCase (npRule "(:[])" (ListCat (CoercCat "Foo" 2)) [Left (CoercCat "Foo" 2)] Parsable)
-- (2,[x]) -> (concatD [prtFoo 2 x])
--
-- >>> mkPrtListCase (npRule "(:)" (ListCat (CoercCat "Foo" 2)) [Left (CoercCat "Foo" 2), Left (ListCat (CoercCat "Foo" 2))] Parsable)
-- (2,x::xs) -> (concatD [prtFoo 2 x ; prtFooListBNFC 2 xs])
--
mkPrtListCase :: Rule -> Doc
mkPrtListCase (Rule f (WithPosition _ (ListCat c)) rhs _)
  | isNilFun f  = parens (precPattern <> "," <> "[]") <+> "->" <+> body
  | isOneFun f  = parens (precPattern <> "," <> "[x]") <+> "->" <+> body
  | isConsFun f = parens (precPattern <> "," <>"x::xs") <+> "->" <+> body
  | otherwise = empty -- (++) constructor
  where
    precPattern = case precCat c of 0 -> "_" ; p -> integer p
    body = text $ mkRhs ["x", "xs"] rhs
mkPrtListCase _ = error "mkPrtListCase undefined for non-list categories"

mkRhs args its =
  "(concatD [" ++ unwords (intersperse ";" (mk args its)) ++ "])"
 where
  mk (arg:args) (Left c : items)  = (prt c +++ arg)        : mk args items
  mk args       (Right s : items) = ("render " ++ mkEsc s) : mk args items
  mk _ _ = []
  prt c = prtFun c +++ show (precCat c)


