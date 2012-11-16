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
import Data.Matrix
import qualified Data.Matrix.Quad as Q
import Data.Matrix.Class
import qualified Data.Matrix.ValiantPosition as VP

-- interface to charts and generic code on top of it.

class IsChart c where
   single :: AbelianGroupZ a => Pair a -> c a
--    merging  :: RingP a => Bool -> c a -> c a -> c a
   merging' :: RingP a => Bool -> c a -> Pair a -> c a -> c a
   root :: AbelianGroupZ a => c a -> a

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



-- instance IsChart MT where
--     root x = at (countColumns x - 1) 0 x
--     merging _ x y = V.merge x y
--     single (x :/: y) = quad z (singleton (x + y)) z z 
--        where z = singleton zero
-- This instance needs the Ring constraint


type MT2 a = Q.Q a

-- instance IsChart MT2 where
--     root x = at (countColumns x - 1) 0 x
--     merging = VP.merge
--     merging' a c b = VP.merge' a c b
--     single x = quad z s z z 
--        where z = singleton zero
--              s = O $ singleton (leftOf x) :/: singleton (rightOf x)

instance IsChart Q.SomeTri where
    root = Q.root 
    merging' a c b = Q.mergein a c b
    single x = Q.single x
