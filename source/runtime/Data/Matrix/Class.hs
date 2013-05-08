{-# LANGUAGE FlexibleInstances #-}

module Data.Matrix.Class where

import Prelude ()
import Algebra.RingUtils
import Control.Applicative hiding ((<|>))


fingerprint m = [[ if isZero (at i j m) then ' ' else 'X' | i <- [0..x-1] ] | j <- [0..y-1]]
  where x = countColumns m
        y = countRows m

(f *** g) (x,y) = (f x,g y)

data Dimension
    = XD
    | YD
      deriving (Eq,Show)

quad a b c d = (a <|> b) <-> (c <|> d)


nextDim XD = YD
nextDim YD = XD

type Extent = (Int,Int)

ext XD (x,y) = x
ext YD (x,y) = y

glueExt XD (x1,y1) (x2,y2)  = (x1+x2,y1)
glueExt YD (x1,y1) (x2,y2)  = (x1,y1+y2)

splitExt XD k (x,y) = ((k,y),(x-k,y))
splitExt YD k (x,y) = ((x,k),(x,y-k))

class Matrix m where
  at :: AbelianGroupZ a => Int -> Int -> m a -> a
  extent :: m a -> Extent
  -- | Sigleton matrix
  singleton :: AbelianGroupZ a => a -> m a
  glue :: AbelianGroup a => Dimension -> m a -> m a -> m a
  split :: AbelianGroupZ a => Dimension -> Int -> m a -> (m a, m a)
  zeroMatrix :: AbelianGroup a => Int -> Int -> m a

instance Matrix m => Matrix (O Pair m) where
  at i j (O (x :/: y)) = at i j x + at i j y
  extent (O (x :/: y)) = extent x -- union with y
  glue d (O p) (O q) = O $ glue d <$> p <*> q
  split d k (O (x :/: y)) = (O $ ax :/: ay, O $ bx :/: by)
     where (ax,bx) = split d k x
           (ay,by) = split d k y
  zeroMatrix x y = O $ pure (zeroMatrix x y)
  singleton x = O $ pure (singleton x) -- Attention: on both sides always!


(<|>) :: (AbelianGroup a, Matrix m) => m a -> m a -> m a
(<|>) = glue XD

(<->) :: (AbelianGroup a, Matrix m)  => m a -> m a -> m a
(<->) = glue YD

countColumns, countRows :: Matrix m => m a -> Int
countColumns = ext XD . extent
countRows = ext YD . extent

chopLastColumn, chopFirstRow, chopFirstColumn, chopLastRow, lastColumn, firstRow :: (AbelianGroupZ a, Matrix m) => m a -> m a
chopFirstRow = snd . split YD 1
chopFirstColumn = snd . split XD 1
chopLastColumn x = fst . split XD (countColumns x - 1) $ x
firstRow = fst . split YD 1
lastColumn x = snd . split XD (countColumns x - 1) $ x

chopLastRow x = fst . split YD (countRows x - 1) $ x






