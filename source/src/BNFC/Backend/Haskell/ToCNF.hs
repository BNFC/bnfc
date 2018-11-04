{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP #-}
{-
    Copyright (C) 2012  Authors:
    Jean-Philippe Bernardy.

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

{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Haskell.ToCNF (generate, genTestFile, genBenchmark) where

{-

Construction of CYK tables. The algorithm follows:

Lange, Martin; Leiß, Hans (2009), "To CNF or not to CNF? An Efficient
Yet Presentable Version of the CYK Algorithm", Informatica Didactica

-}
import BNFC.ToCNFCore
import BNFC.CF hiding (App,Exp)
import BNFC.Backend.Haskell.HsOpts
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (Const)
#endif
import qualified Data.Map as M
import Data.Monoid
import Data.Pair
import Text.PrettyPrint.HughesPJ hiding (first,(<>))

-- Code generation

incomment x = "{-" <> x <> "-}"

generate opts cf0 = render $ vcat [header opts
                                  ,genShowFunction cf0
                                  ,genCatTags cf1
                                  ,genDesc cf1 descriptions
                                  ,genNeighborSet neighbors
                                  ,genCombTable units (onRules (filter (not . isUnitRule)) cf)
                                  ,genTokTable units cf
                                  ,incomment $ vcat
                                   ["Normalised grammar:"
                                   ,text $ show cf
                                   ,"Unit relation:"
                                   ,prettyUnitSet units
                                   ]
                                  ]
  where (cf1,cf,units,descriptions,neighbors) = toCNF cf0

class Pretty a where
  pretty :: a -> Doc

instance (Pretty k, Pretty v) => Pretty (Set k v) where
  pretty s = sep [pretty k <> " --> " <> pretty v | (k,x) <- M.assocs s, v <- x]

instance Pretty (Either Cat String) where
  pretty (Left x) = text $ show x
  pretty (Right x) = quotes $ text x

instance Pretty String where
  pretty = text

prettyUnitSet units = vcat [prettyExp f <> " : " <> catTag cat <> " --> " <> text (show cat') | (cat,x) <- M.assocs units, (f,cat') <- x]

header opts
       = vcat ["{-# LANGUAGE MagicHash, FlexibleInstances #-}"
              ,"module " <> text (cnfTablesFileM opts) <> " where"
              ,"import GHC.Prim"
              ,"import Control.Applicative hiding (Const)"
              ,"import Algebra.RingUtils"
              ,"import Parsing.Chart ()"
              ,"import " <> text (absFileM  opts)
              ,"import " <> text (alexFileM opts)
              ,"import " <> text ( printerFileM  opts)
              ,"readInteger :: String -> Integer"
              ,"readInteger = read"
              ,"readDouble :: String -> Double"
              ,"readDouble = read"
              ,"instance RingP [(CATEGORY,Any)] where"
              ,"  mul p a b = trav [map (app tx ty) l :/: map (app tx ty) r | (x,tx) <- a, (y,ty) <- b, let l:/:r = combine p x y]"
              ,"    where trav :: [Pair [a]] -> Pair [a]"
              ,"          trav [] = pure []"
              ,"          trav (x:xs) = (++) <$> x <*> trav xs"
              ,"          app tx ty (z,f)  = (z, f tx ty)"
              ]

genShowFunction cf = hang "showAst (cat,ast) = case cat of " 6
       (vcat [catTag (Left cat) <> " -> printTree ((unsafeCoerce# ast)::" <> text (show cat) <> ")"
             | cat <- filter isDataCat $ allCats cf] $$
        "_ -> \"Unprintable category\"")


genCatTags :: CFG Exp -> Doc
genCatTags cf = "data CATEGORY = " <> punctuate' "|" (map catTag (allSyms cf)) $$
                "  deriving (Eq,Ord,Show)"

genDesc :: CFG Exp -> CatDescriptions -> Doc
genDesc cf descs = vcat ["describe " <> catTag s <> " = " <> text (show (descOf s)) | s <- allSyms cf]
  where descOf :: Either Cat String -> String
        descOf (Right x) = "token " <> x
        descOf (Left x) = maybe (show x) render $ M.lookup x descs

genCombTable :: UnitRel Cat -> CFG Exp -> Doc
genCombTable units cf =
     "combine :: Bool -> CATEGORY -> CATEGORY -> Pair [(CATEGORY, Any -> Any -> Any)]"
  $$ genCombine units cf
  $$ "combine _ _ _ = pure []"

allSyms :: CFG Exp -> [Either Cat String]
allSyms cf = map Left (allCats cf  ++ literals cf) ++ map (Right . fst) (cfTokens cf)


ppPair (x,y) = parens $ x <> comma <> " " <> y

unsafeCoerce' = app' (Con "unsafeCoerce#")


prettyPair (x :/: y) = sep [x,":/:",y]
prettyListFun xs = parens $ sep (map (<> "$") xs) <> "[]"


genCombine :: UnitRel Cat -> CFG Exp -> Doc
genCombine units cf = vcat $ map genEntry $ group' $ map (alt units) (cfgRules cf)
  where genEntry :: ((RHSEl,RHSEl),[(Cat,Exp)]) -> Doc
        genEntry ((r1,r2),cs) = "combine p " <> catTag r1 <> " " <> catTag r2 <> " = " <> prettyPair (genList <$> splitOptim (Left . fst) cf cs)
        mkLam body = "\\x y -> " <> body
        genList xs = prettyListFun [p (ppPair (catTag . Left $ x, mkLam . prettyExp . unsafeCoerce' $ y)) | ((x,y),p) <- xs]

alt :: UnitRel Cat -> Rul Exp -> ((RHSEl,RHSEl),[(Cat,Exp)])
alt units (Rule f c [r1,r2]) = ((r1,r2),initial:others)
  where initial = (c, f `appMany` args)
        others = [(c', f' `app'` (f `appMany` args)) | (f',c') <- lookupMulti (Left c) units]
        args = map (unsafeCoerce' . Con) $ ["x"|isCat r1]++["y"|isCat r2]
alt _ _ = error "Only works with binary rules"



genTokTable :: UnitRel Cat -> CFG Exp -> Doc
genTokTable units cf = "tokenToCats :: Bool -> Token -> Pair [(CATEGORY,Any)]" $$
                       vcat (map (genSpecEntry cf units) (tokInfo cf)) $$
                       vcat (map (genTokEntry cf units) (cfTokens cf)) $$
                       "tokenToCats p t = error (\"unknown token: \" ++ show t)"

tokInfo cf = (catChar,"TC",Con "head"):
             (catString,"TL",Id):
             (catInteger,"TI",Con "readInteger"):
             (catDouble,"TD",Con "readDouble"):
             [(catIdent,"TV",Con "Ident")|hasIdent cf] ++
             [(t,"T_" <> text (show t),(Con (show t))) | (t,_) <- tokenPragmas cf]

genTokCommon cf xs = prettyPair (gen <$> splitOptim fst cf xs)
  where gen ys = prettyListFun [p (ppPair (catTag x,y)) | ((x,y),p) <- ys]

genSpecEntry cf units (tokName,constrName,fun) = "tokenToCats p (PT (Pn _ l c) (" <> constrName <> " x)) = " <> genTokCommon cf xs
  where xs = map (second (prettyExp . (\f -> unsafeCoerce' (f `app'` tokArgs)))) $
             (Left tokName, fun) : [(Left c,f `after` fun) | (f,c) <- lookupMulti (Left tokName) units]
        tokArgs | isPositionCat cf tokName = Con "((l,c),x)"
                | otherwise = Con "x"

genTokEntry cf units (tok,x) =
  " -- " <> text tok $$
  "tokenToCats p (PT posn (TS _ " <> int x <> ")) = " <> genTokCommon cf xs
  where xs = (Right tok, tokVal) :
             [(Left c,prettyExp (unsafeCoerce' f)) | (f,c) <- lookupMulti (Right tok) units]
        tokVal = "error" <> (text $ show $ "cannot access value of token: " ++ tok)

ppList = brackets . punctuate' ", "

genNeighborSet ns = vcat
              ["neighbors " <> catTag x <> " = " <> ppList (map catTag y)
              | (x,y) <- ns] $$
               "neighbors _ = []"

------------------------
-- Test file generation

genTestFile opts _ = render $ vcat
    ["module Main where"
    ,"import " <> text ( alexFileM     opts)
    ,"import " <> text ( cnfTablesFileM opts)
    ,"import Parsing.TestProgram"
    ,"main = mainTest showAst tokenToCats tokens tokenLineCol describe neighbors"]


genBenchmark opts = render $ vcat
   ["import System.Environment ( getArgs )"
   ,"import "<> text ( alexFileM opts) <> " as Lexer"
   ,"import "<> text ( cnfTablesFileM opts) <> " as Parser"
   ,"import GHC.Exts"
   ,"import Parsing.Chart"
   ,"import Criterion.Main"
   ,"import Algebra.RingUtils"
   ,"import Control.Applicative"
   ,"type T = [(CATEGORY,Any)]"
   ,"pLGrammar :: [Pair T] -> MT2 T"
   ,"pLGrammar = mkTree"
   ,"main = do"
   ,"  f:_ <- getArgs"
   ,"  s <- readFile f"
   ,"  let ts = zipWith tokenToCats (cycle [False,True]) (Lexer.tokens s)"
   ,"      (ts1,x:ts2) = splitAt (length ts `div` 2) ts"
   ,"      cs = [mkTree ts1,mkTree' ts2]"
   ,"      work [c1,c2] = show $ map fst $ root $ mergein False c1 x c2"
   ,"  defaultMain [bench f $ nf work cs] -- note the hack!!!"
   ]
