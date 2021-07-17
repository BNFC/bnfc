{-
    BNF Converter: GADT Abstract syntax Generator
    Copyright (C) 2004-2005  Author:  Markus Forsberg, Björn Bringert

-}

{-# LANGUAGE PatternGuards #-}

module BNFC.Backend.HaskellGADT.CFtoAbstractGADT (cf2Abstract) where

import qualified Data.List as List

import BNFC.CF
import BNFC.Backend.HaskellGADT.HaskellGADTCommon
import BNFC.Backend.Haskell.Utils
import BNFC.Backend.Haskell.CFtoAbstract (definedRules)
import BNFC.Options
import BNFC.Utils ((+++), when)


cf2Abstract :: TokenText -> String -> CF -> String -> String
cf2Abstract tokenText name cf composOpMod = unlines $ concat $
  [ [ "-- For GHC version 7.10 or higher"
    , ""
    , "{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}"
    ]
  , [ "{-# LANGUAGE EmptyCase #-}" | emptyTree ]
  , [ "{-# LANGUAGE LambdaCase #-}"
    , ""
    , "{-# OPTIONS_GHC -fno-warn-unused-binds #-}"
      -- unused-local-binds would be sufficient, but parses only from GHC 8.0
    , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    , "{-# OPTIONS_GHC -fno-warn-unused-matches #-}"
    , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
      -- defects of coverage checker, e.g. in 8.2.2, may lead to warning
      -- about exceeded iterations for pattern match checker
    , "{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}"
    , ""
    , "module" +++ name +++ "(" ++ List.intercalate ", " exports ++ ")" +++ "where"
    , ""
    , "import Prelude (" ++ typeImports ++ ", (.), (>), (&&), (==))"
    , "import qualified Prelude as P"
    -- , "import qualified Data.Kind as K (Type)"
    ]
  , tokenTextImport tokenText
  , [ ""
    , "import " ++ composOpMod
    , ""
    ]
  , prDummyTypes cf
  , [""]
  , prTreeType tokenText cf
  , [""]
  , prCompos cf
  , [""]
  , prShow cf
  , [""]
  , prEq cf
  , [""]
  , prOrd cf
  , [""]
  , map ((++ "\n") . show) $ definedRules False cf
  ]
  where
    emptyTree = null (cf2cons cf)
    exports = concat $
      [ [ if emptyTree then "Tree" else "Tree(..)" ]
      , getTreeCats cf
      , map mkDefName $ getDefinitions cf
      , [ "johnMajorEq"
        , "module " ++ composOpMod
        ]
      ]
    typeImports = List.intercalate ", " $ concat
      [ [ "Char", "Double" ]
      , [ "Int" | hasPositionTokens cf ]
      , [ "Integer", "String" ]
      ]

getTreeCats :: CF -> [String]
getTreeCats cf = List.nub $ map catToStr $ filter (not . isList) $ map consCat $ cf2cons cf

getDefinitions :: CF -> [String]
getDefinitions = map (funName . defName) . definitions

prDummyTypes :: CF -> [String]
prDummyTypes cf = prDummyData : map prDummyType cats
  where
  cats = getTreeCats cf
  prDummyData
    | null cats = "data Tag"
    | otherwise = "data Tag =" +++ List.intercalate " | " (map mkRealType_ cats)
  prDummyType cat = "type" +++ cat +++ "= Tree" +++ mkRealType cat

-- | Use in occurrences of promoted constructors.
--
-- Promoted constructors should be preceded by a prime,
-- otherwise we get GHC warning @unticked-promoted-constructors@.
mkRealType :: String -> String
mkRealType cat = "'" ++ mkRealType_ cat

-- | Use in @data@ definition (for the sake of GHC <= 8.6).
mkRealType_ :: String -> String
mkRealType_ cat = cat ++ "_"


prTreeType :: TokenText -> CF -> [String]
prTreeType tokenText cf =
  "data Tree (a :: Tag) where" : map (("    " ++) . prTreeCons) (cf2cons cf)
  where
  prTreeCons c
      | TokenCat tok <- cat, isPositionCat cf tok =
          fun +++ ":: ((Int,Int),"++ tokenTextType tokenText ++") -> Tree" +++ mkRealType tok
      | otherwise =
          fun +++ "::" +++ concat [catToStr c +++ "-> " | (c,_) <- consVars c] ++ "Tree" +++ mkRealType (catToStr cat)
    where
    (cat,fun) = (consCat c, consFun c)

prCompos :: CF -> [String]
prCompos cf =
    ["instance Compos Tree where",
     "  compos r a f = \\case"]
    ++ map ("      "++) (concatMap prComposCons cs
                         ++ ["t -> r t" | not (all isRecursive cs)])
  where
    cs = cf2cons cf
    prComposCons c
        | isRecursive c = [consFun c +++ unwords (map snd (consVars c)) +++ "->" +++ rhs c]
        | otherwise = []
    isRecursive c = any (isTreeType cf) (map fst (consVars c))
    rhs c = "r" +++ consFun c +++ unwords (map prRec (consVars c))
      where prRec (cat,var) | not (isTreeType cf cat) = "`a`" +++ "r" +++ var
                            | isList cat = "`a` P.foldr (\\ x z -> r (:) `a` f x `a` z) (r [])" +++ var
                            | otherwise = "`a`" +++ "f" +++ var

prShow :: CF -> [String]
prShow cf = ["instance P.Show (Tree c) where",
              "  showsPrec n = \\case"]
              ++ map (("    "++) .prShowCons) cs
              ++ ["    where",
                  "    opar = if n > 0 then P.showChar '(' else P.id",
                  "    cpar = if n > 0 then P.showChar ')' else P.id"]
  where
    cs = cf2cons cf
    prShowCons c | null vars = fun +++ "->" +++ "P.showString" +++ show fun
                 | otherwise = fun +++ unwords (map snd vars) +++ "->"
                                   +++ "opar . P.showString" +++ show fun
                                   +++ unwords [". P.showChar ' ' . P.showsPrec 1 " ++ x | (_,x) <- vars]
                                   +++ ". cpar"
      where (fun, vars) = (consFun c, consVars c)

prEq :: CF -> [String]
prEq cf = ["instance P.Eq (Tree c) where (==) = johnMajorEq",
           "",
           "johnMajorEq :: Tree a -> Tree b -> P.Bool"]
           ++ map prEqCons (cf2cons cf)
           ++ ["johnMajorEq _ _ = P.False"]
  where prEqCons c
            | null vars = "johnMajorEq" +++ fun +++ fun +++ "=" +++ "P.True"
            | otherwise = "johnMajorEq" +++ "(" ++ fun +++ unwords vars ++ ")"
                          +++ "(" ++ fun +++ unwords vars' ++ ")" +++ "="
                          +++ List.intercalate " && " (zipWith (\x y -> x +++ "==" +++ y) vars vars')
          where (fun, vars) = (consFun c, map snd (consVars c))
                vars' = map (++ "_") vars

prOrd :: CF -> [String]
prOrd cf = concat
  [ [ "instance P.Ord (Tree c) where"
    , "  compare x y = P.compare (index x) (index y) `P.mappend` compareSame x y"
    ]
  , [ "", "index :: Tree c -> P.Int" ]
  , zipWith mkIndex cs [0..]
  , when (null cs) [ "index = P.undefined" ]
  , [ "", "compareSame :: Tree c -> Tree c -> P.Ordering" ]
  , map mkCompareSame cs
  -- Case sometimes redundant, so we need to suppress the warning.
  , [ "compareSame _ _ = P.error \"BNFC error: compareSame\"" ]
  ]
  where cs = cf2cons cf
        mkCompareSame c
            | null vars = "compareSame" +++ fun +++ fun +++ "=" +++ "P.EQ"
            | otherwise = "compareSame" +++ "(" ++ fun +++ unwords vars ++ ")"
                          +++ "(" ++ fun +++ unwords vars' ++ ")" +++ "="
                          +++ foldr1 (\x y -> "P.mappend (" ++ x ++") ("++y++")") cc
            where (fun, vars) = (consFun c, map snd (consVars c))
                  vars' = map (++"_") vars
                  cc = zipWith (\x y -> "P.compare"+++x+++y) vars vars'
        mkIndex c i = "index" +++ "(" ++ consFun c
                       +++ unwords (replicate (length (consVars c)) "_") ++ ")"
                       +++ "=" +++ show i
