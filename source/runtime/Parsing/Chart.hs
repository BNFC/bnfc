{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}

module Parsing.Chart where

import Data.Array
import Data.Maybe
import Prelude ()
import Data.Traversable (sequenceA)
import Control.Applicative ((<$>),(<*>),pure)
import Control.Monad(join)

import Data.List (splitAt)
import Algebra.RingUtils
import qualified Data.Matrix.Quad as Q
import Data.Matrix.Class

fingerprint = Q.fingerprint

{-
mkTreeHelp alt s = sweeps (map single s)
 where
  sweeps []  = error "can't parse the empty string, sorry"
  sweeps [p] = p
  sweeps ps  = sweeps (pairs ps alts)

  pairs []  _       = []
  pairs [p] _      = [p]
  pairs (p:q:ps) (b:bs) = (merging b p q) : pairs ps bs

  alts = cycle alt
  -}

-- mkTree2 :: (AbelianGroupZ (c a), RingP a, IsChart c) => Bool -> [Pair a] -> c a
mkTree2 :: RingP a => Bool -> [Pair a] -> Q.Q a
mkTree2 p [] = error "can't parse the empty string, sorry"
mkTree2 p [x] = Q.square2 x
mkTree2 p [x,y] = Q.square3 p x y
mkTree2 p leaves = Q.mergein p (mkTree2 False xs) y (mkTree2 True zs)
 where (xs,y:zs) = splitAt n2 leaves
       n2 = length leaves `div` 2


-- mkTree :: (RingP a, IsChart c) => [Pair a] -> c a
mkTree = mkTree2 False -- mkTreeHelp [False,True]
mkTree' = mkTree2 True -- mkTreeHelp [True,False]


type Set a = [a]

-- Sets form an abelian group
instance AbelianGroup (Set a) where
    zero = []
    (+) = (++)

instance AbelianGroupZ (Set a) where
    isZero = null

type MT2 a = Q.Q a

genXPM xs@(h:_) = unlines $
  ["! XPM2",
   -- <width/cols> <height/rows> <colors> <char on pixel>
   show width ++ " " ++ show height ++ " 4 1",
   "X c cyan",
   "< c blue",
   "> c red",
   "  c black"
   ] ++
   xs
  where width = length h
        height = length xs

root = Q.root
mergein a c b = Q.mergein a c b
single x = Q.single x

