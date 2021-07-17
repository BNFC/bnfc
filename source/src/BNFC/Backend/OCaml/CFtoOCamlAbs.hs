{-# LANGUAGE LambdaCase #-}

{-
    BNF Converter: OCaml Abstract Syntax Generator
    Copyright (C) 2005  Author:  Kristofer Johannisson

-}

-- based on BNFC Haskell backend

module BNFC.Backend.OCaml.CFtoOCamlAbs (cf2Abstract) where

import Text.PrettyPrint

import BNFC.CF
import BNFC.Utils ( (+++), unless, parensIf )
import Data.List  ( intersperse )
import BNFC.Backend.OCaml.OCamlUtil

-- to produce an OCaml module
cf2Abstract :: String -> CF -> String
cf2Abstract _ cf = unlines $ concat
  [ mutualRecDefs $ concat
    [ map (prSpecialData cf) (specialCats cf)
    , map prData (cf2data cf)
    ]
  , unless (null defs) $ concat
    [ [ "(* defined constructors *)"
      , ""
      ]
    , defs
    ]
  ]
  where
  defs = definedRules cf

definedRules :: CF -> [String]
definedRules cf = map mkDef $ definitions cf
  where
    mkDef (Define f args e _) =
      "let " ++ sanitizeOcaml (funName f) ++ " " ++ mkTuple (map fst args) ++ " = " ++ ocamlExp False e

    ocamlExp :: Bool -> Exp -> String
    ocamlExp p = \case
      Var s       -> s
      App "(:)" _ [e1, e2] -> parensIf p $ unwords [ ocamlExp True e1, "::", ocamlExp False e2 ]
      App s _ []  -> sanitizeOcaml s
      App s _ [e] -> parensIf p $ sanitizeOcaml s ++ ' ' : ocamlExp True e
      App s _ es  -> parensIf p $ sanitizeOcaml s ++ ' ' : mkTuple (map (ocamlExp False) es)
      LitInt i    -> show i
      LitDouble d -> show d
      LitChar c   -> "\'" ++ c : "\'"
      LitString s -> "\"" ++ s ++ "\""

-- allow mutual recursion so that we do not have to sort the type definitions in
-- dependency order
mutualRecDefs :: [String] -> [String]
mutualRecDefs ss = case ss of
    [] -> []
    [x] -> ["type" +++ x]
    x:xs -> ("type" +++ x)  :  map ("and" +++) xs



prData :: Data -> String
prData (cat,rules) =
  fixType cat +++ "=\n   " ++
  concat (intersperse "\n | " (map prRule rules)) ++
  "\n"

prRule (fun,[])   = fun
prRule (fun,cats) = fun +++ "of" +++ render (mkTupleType cats)

-- | Creates an OCaml type tuple by intercalating * between type names
-- >>> mkTupleType [Cat "A"]
-- a
--
-- >>> mkTupleType [Cat "A", Cat "Abc", Cat "S"]
-- a * abc * s
mkTupleType :: [Cat] -> Doc
mkTupleType = hsep . intersperse (char '*') . map (text . fixType)

prSpecialData :: CF -> TokenCat -> String
prSpecialData cf cat = fixType (TokenCat cat) +++ "=" +++ cat +++ "of" +++ contentSpec cf cat

--  unwords ["newtype",cat,"=",cat,contentSpec cf cat,"deriving (Eq,Ord,Show)"]

contentSpec :: CF -> TokenCat -> String
contentSpec cf cat = -- if isPositionCat cf cat then "((Int,Int),String)" else "String"
    if isPositionCat cf cat then "((int * int) * string)" else "string"
