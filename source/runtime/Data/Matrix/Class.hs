{-# LANGUAGE FlexibleInstances #-}

module Data.Matrix.Class where

import Prelude ()
import Algebra.RingUtils
import Control.Applicative

(f *** g) (x,y) = (f x,g y)

data Dimension 
    = XD  
    | YD
      deriving (Eq,Show)


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
 
 
{-           
instance AbelianGroupZ a => AbelianGroupZ (Y.Matrix a) where
  isZero (Y.Matrix xs) = all (all isZero) xs
                 
instance Matrix (Y.Matrix) where
  atZero (Y.Matrix ((x:_):_)) = x
  extent (Y.Matrix []) = (0,0)
  extent (Y.Matrix (x:xs)) = (length x, 1 + length xs)
  glue XD (Y.Matrix x) (Y.Matrix y) = Y.Matrix (zipWith (++) x y)
  glue YD (Y.Matrix x) (Y.Matrix y) = Y.Matrix (x ++ y)
  split XD k (Y.Matrix x) = (Y.Matrix *** Y.Matrix) (unzip (map (splitAt k) x))
  split YD k (Y.Matrix x)= (Y.Matrix *** Y.Matrix) (splitAt k x)
  singleton x = Y.Matrix [[x]]
  zeroMatrix columns rows = Y.Matrix $ replicate rows (replicate columns zero)
-}

-- at :: (AbelianGroupZ a, Matrix m) => Int -> Int -> m a -> a
-- at x y m = atZero $ snd $ split XD x $ snd $ split YD y $ m
  

  
  
  