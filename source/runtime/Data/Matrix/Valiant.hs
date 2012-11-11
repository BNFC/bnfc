module Data.Matrix.Valiant where
import Prelude ()

import qualified Data.List as L

import Data.Matrix.Class
import Algebra.RingUtils
import Control.Applicative (pure, (<$>))

import Data.Matrix.Tree

closeOverlap :: (Matrix m, Ring (m a), AbelianGroupZ a) =>
                (m a,m a) -> m a -> (m a,m a) -> m a
closeOverlap (a,x) m (y,b) = closeDisjoint a (m + x*y) b 
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

closeDisjoint :: (Matrix m, Ring (m a), AbelianGroupZ a) =>
                 m a -> m a -> m a -> m a
closeDisjoint l m r 
    | isZero m = m
    | isZero l && isZero r = m
    | otherwise = (z2 <|> z4) <-> -- l r triangular
                  (z1 <|> z3)
  where p = countRows l
        q = countRows r
        p' = p `div` 2
        q' = q `div` 2
        (m1,m2) = split YD p' m
        (v1,v3) = split XD q' m2
        (v2,v4) = split XD q' m1
        (a,x,b) = split3t p' l
        (c,y,d) = split3t q' r
        z1 = closeDisjoint b v1 c
        z2 = closeOverlap (a,x ) v2 (z1,c)
        z3 = closeOverlap (b,z1) v3 (y ,d)
        z4 = closeOverlap (a, x <|> z2) v4 (z3 <-> y, d)

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

merge :: (Matrix m, Ring (m a), AbelianGroupZ a) => m a -> m a -> m a
merge l r = (l' <|> m ) <->
            (z' <|> r')
    where l' = chopLastRow l     -- excess zeros
          r' = chopFirstColumn r -- excess zeros
          (a,b) = split XD (countColumns l' - 1) l' 
          (c,d) = split YD 1                     r'
          -- m = closeOverlap (a,b) z (c,d) ; z = zeroMatrix (countColumns r - 1) (countRows l - 1)
          m = closeDisjoint a (b*c) d
          z' = zeroMatrix (countColumns l) (countRows r)

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




