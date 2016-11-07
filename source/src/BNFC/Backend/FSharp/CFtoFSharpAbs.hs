{-
    BNF Converter: FSharp Abstract Syntax Generator
    Copyright (C) 2021  Author:  Grzegorz Dziadkiewicz

-}

-- based on BNFC OCaml backend

module BNFC.Backend.FSharp.CFtoFSharpAbs (cf2Abstract) where

import Text.PrettyPrint

import BNFC.CF
import BNFC.Utils ( (+++), unless, parensIf )
import Data.List  ( intersperse, intercalate )
import BNFC.Backend.FSharp.FSharpUtil

-- to produce an F# module
cf2Abstract :: String -> CF -> String
cf2Abstract absMod cf = unlines $ concat
  [ ["module" +++ absMod]
  , mutualRecDefs $ concat
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
      "let " ++ sanitizeFSharp (funName f) ++ " " ++ mkTuple (map fst args) ++ " = " ++ fsharpExp False e

    fsharpExp :: Bool -> Exp -> String
    fsharpExp p = \case
      Var s       -> s
      App "(:)" _ [e1, e2] -> parensIf p $ unwords [ fsharpExp True e1, "::", fsharpExp False e2 ]
      App s _ []  -> sanitizeFSharp s
      App s _ [e] -> parensIf p $ sanitizeFSharp s ++ ' ' : fsharpExp True e
      App s _ es  -> parensIf p $ sanitizeFSharp s ++ ' ' : mkTuple (map (fsharpExp False) es)
      LitInt i    -> show i
      LitDouble d -> show d
      LitChar c   -> "\'" ++ c : "\'"
      LitString s -> "\"" ++ s ++ "\""

-- allow mutual recursion so that we do not have to sort the type definitions in
-- dependency order
mutualRecDefs :: [String] -> [String]
mutualRecDefs [] = []
mutualRecDefs (x:xs) = ("type" +++ x)  :  map ("and" +++) xs

prData :: Data -> String
prData (cat,rules) =
  fixType cat +++ "=\n   " ++
  intercalate "\n | " (map prRule rules) ++
  "\n"

prRule (fun, [])  = fun
prRule (fun,cats) = fun +++ "of" +++ render (mkTupleType cats)

-- | Creates an OCaml type tuple by intercalating * between type names
-- >>> mkTupleType [Cat "A"]
-- A
--
-- >>> mkTupleType [Cat "A", Cat "Abc", Cat "S"]
-- A * Abc * S
mkTupleType :: [Cat] -> Doc
mkTupleType = hsep . intersperse (char '*') . map (text . fixType)

prSpecialData :: CF -> TokenCat -> String
prSpecialData cf cat = fixType (TokenCat cat) +++ "=" +++ cat +++ "of" +++ contentSpec cf cat

--  unwords ["newtype",cat,"=",cat,contentSpec cf cat,"deriving (Eq,Ord,Show)"]

contentSpec :: CF -> TokenCat -> String
contentSpec cf cat
   | isPositionCat cf cat = "((int * int) * string)"
   | otherwise = "string"
