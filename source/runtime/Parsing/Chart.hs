{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances #-}

module Parsing.Chart where

import Data.Array
import Data.Maybe
import Prelude ()
import Data.Traversable (sequenceA)
import Control.Applicative
import Control.Monad(join)

-- import Parsing.CNF
import Algebra.RingUtils
import Data.Matrix
import Data.Matrix.Class
import Data.Matrix.Valiant

-- interface to charts and generic code on top of it.

class IsChart c where
    single :: AbelianGroupZ a => Pair a -> c a
    merging :: RingP a => Bool -> c a -> c a -> c a
    root :: AbelianGroupZ a => c a -> a



mkTreeHelp alt s = sweeps (map single s)
 where
  sweeps []  = error "can't parse the empty string, sorry"
  sweeps [p] = p
  sweeps ps  = sweeps (pairs ps alts)

  pairs []  _       = []
  pairs [p] _      = [p]
  pairs (p:q:ps) (b:bs) = (merging b p q) : pairs ps bs

  alts = cycle alt
  
mkTree :: (RingP a, IsChart c) => [Pair a] -> c a
mkTree = mkTreeHelp [False,True]  
mkTree' = mkTreeHelp [True,False]  

{-
powerN p 0 t = unitChart p t
powerN p n t = merging p (powerN False (n-1) t) (powerN True (n-1) t)

powers n t = listArray (0,n) pows
   where pows = (unitChart False t, unitChart True t) : map (\(x,y) -> (merging False x y, merging True x y)) pows 

-}

type Set a = [a]

-- Sets form an abelian group
instance AbelianGroup (Set a) where
    zero = []
    (+) = (++)

instance AbelianGroupZ (Set a) where
    isZero = null



instance IsChart MT where
    root x = at (countColumns x - 1) 0 x
    merging _ x y = merge x y
    single (x :/: y) = quad z (singleton (x + y)) z z 
       where z = singleton zero


