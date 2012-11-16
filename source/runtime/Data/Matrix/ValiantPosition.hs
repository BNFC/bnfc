{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, GADTs, RankNTypes, TypeOperators, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module Data.Matrix.ValiantPosition where
import Prelude ()

import qualified Data.List as L

import Data.Matrix.Class
import Algebra.RingUtils
import Control.Applicative (pure, (<$>))

import Data.Matrix.Tree


type Pai m a = (Pair `O` m) a

lef = leftOf . fromO
rig = rightOf . fromO


closeOverlap :: (Matrix m, RingP (m a), AbelianGroupZ a) =>
                Bool -> (m a,m a) -> Pai m a -> (m a,m a) -> Pai m a
closeOverlap p (a,x) m (y,b) = closeDisjoint p a (m + O (mul p x y)) b 
 -- a,b triangular

{-
a a a a x x x m m
a a a a x x x m m
a a a a x x x m m
a a a a x x x m m
              y y
              y y
              y y
              b b
              b b
-}

closeDisjoint :: (Matrix m, RingP (m a), AbelianGroupZ a) =>
                 Bool -> m a -> Pai m a -> m a -> Pai m a
closeDisjoint p l m r 
    | isZero m = m
    | isZero l && isZero r = m
    | otherwise = (z2 <|> z4) <-> -- l r triangular
                  (z1 <|> z3)
  where p' = countRows l `div` 2
        q' = countRows r `div` 2
        (m1,m2) = split YD p' m
        (v1,v3) = split XD q' m2
        (v2,v4) = split XD q' m1
        (a,x,b) = split3t p' l
        (c,y,d) = split3t q' r
        z1 = closeDisjoint p b v1 c
        z2 = closeOverlap p (a,x) v2 (rig z1,c)
        z3 = closeOverlap p (b,lef z1) v3 (y ,d)
        z4 = closeOverlap p (a, x <|> lef z2) v4 (rig z3 <-> y, d)

{-
  a a a a x x x 2 2 4 
    a a a x x x 2 2 4
      a a x x x 2 2 4
        a x x x 2 2 4
          b b b 1 1 3
            b b 1 1 3
              b 1 1 3
                c c y
                  c y
                    d
-}

mergeBench :: forall m a. (Matrix m, RingP (m a), AbelianGroupZ a) => Bool -> Pai m a -> Pai m a -> Pai m a
mergeBench p l r = m
    where l' = chopLastRow l     -- excess zeros
          r' = chopFirstColumn r -- excess zeros
          (a,b) = split XD (countColumns l' - 1) l' 
          (c,d) = split YD 1                     r'
          m = closeDisjoint p (lef a) (O $ mul p (lef b) (rig c)) (rig d)

merge :: forall m a. (Matrix m, RingP (m a), AbelianGroupZ a) => Bool -> Pai m a -> Pai m a -> Pai m a
merge p l r = (l' <|> m ) <->
              (z' <|> r')
    where l' = chopLastRow l     -- excess zeros
          r' = chopFirstColumn r -- excess zeros
          (a,b) = split XD (countColumns l' - 1) l' 
          (c,d) = split YD 1                     r'
          -- m = closeOverlap (a,b) z (c,d) ; z = zeroMatrix (countColumns r - 1) (countRows l - 1)
          m = closeDisjoint p (lef a) (O $ mul p (lef b) (rig c)) (rig d)
          z' = zeroMatrix (countColumns l) (countRows r)

merge' :: forall m a. (Matrix m, RingP (m a), AbelianGroupZ a) => Bool -> Pai m a -> Pai m a -> Pai m a -> Pai m a
merge' p a c b = (a <|> x ) <->
                 (z <|> b )
    where x = closeDisjoint p (lef a) c (rig b)
          z = zeroMatrix (countColumns a) (countRows b)


{-
a a a b
  a a b
    a b
      0 c c c    
        d d d    
          d d    
            d
-}

split3t k a = (l,m,r)
    where (x,b) = split YD k a
          (l,m) = split XD k x
          (z,r) = split XD k b

{-

l l l l m m  \
  l l l m m  |  x
    l l m m  |  
      l m m  /
        r r  \  b
          r  /

-}

