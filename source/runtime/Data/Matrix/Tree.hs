{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, GADTs, RankNTypes #-}

module Data.Matrix.Tree (($$$),
              PTree(..), splitAt, atP, bin, leaf, 
              -- debug
              columnMatrix, rowMatrix, printT,depth,
              Validable(..),
             ) where

import Prelude ()
import qualified Data.List as L

-- import Test.QuickCheck
import Data.Typeable
import Control.Monad
import Control.Applicative

import Data.Matrix.Class
import Algebra.RingUtils

($$$) :: (a -> b,c -> d) -> (a,c) -> (b,d)
~(f1,f2) $$$ ~(a1,a2) = (f1 a1, f2 a2)


atZ Empty = zero  
atZ (Leaf x) = x
atZ (Bin _ _ l _) = atZ l

atP i j Empty = zero
atP 0 0 (Leaf x) = x
atP _ _ (Leaf _) = zero
atP i j (Bin XD k l r) | i < k = atP i j l
                       | otherwise = atP (i-k) j r
atP i j (Bin YD k l r) | j < k = atP i j l
                       | otherwise = atP i (j-k) r
                                     
leaf x | isZero x = Empty
       | otherwise = Leaf x

data PTree a
    = Empty
    | Leaf a -- at (0,0)
    | Bin {dim :: Dimension,
           splitPos :: Int,
           left :: PTree a,
           right :: PTree a
          }
      deriving (Show,Functor)

-- | Convert a matrix to a list of rows (for printing)
toRows :: forall a. AbelianGroup a => PTree a -> [[a]]
toRows Empty = []
toRows (Leaf a) = [[a]]
toRows (Bin YD k l r) = pad [] k (toRows l) ++ toRows r
toRows t@(Bin XD k l r) = zipWith (++) (pad (replicate k zero) m lRows) 
                                       (pad []                 m rRows)
   where lRows = map (pad zero k) (toRows l)
         rRows = toRows r
         m = max (length rRows) (length lRows)

-- | pad a list with a given element
pad :: a -> Int -> [a] -> [a]
pad a n x = take n (x ++ repeat a)

-- | Print a matrix
printT :: (AbelianGroup a,Show a) => PTree a -> IO ()
printT x = forM_ (toRows x) $ \r ->
              putStrLn $ L.intercalate "," $ map show r

isEmpty Empty = True
isEmpty _ = False
instance (AbelianGroupZ a, Eq a) => Eq (PTree a) where
    Empty == Empty = True
    Leaf a == Leaf b = a == b
    Empty == Leaf x = isZero x
    Leaf x == Empty = isZero x
    (Bin d k a b) == y = a == a' && b == b' 
       where (a',b') = splitAt k d y
    y == (Bin d k a b) = a == a' && b == b' 
       where (a',b') = splitAt k d y

depth :: PTree a -> Int    
depth (Bin _ _ l r) = 1 + max (depth l) (depth r)
depth _ = 1
  
-- normalise empty space
bin d k x Empty = x
bin d 0 _ x = x
bin XD k Empty (Bin XD k' Empty r) = Bin XD (k+k') Empty r
bin YD k Empty (Bin YD k' Empty r) = Bin YD (k+k') Empty r
bin YD k Empty (Bin XD k' Empty r) = Bin XD k' Empty $ bin YD k Empty r 
bin d k l r = Bin d k l r
-- These two equations are a bad idea: essentially their effect is to linearise the matrix representation.
-- bin d k l (Bin d' k' a b) | d == d' = Bin d' (k+k') (bin d k l a) b
-- bin d k (Bin d' k' a b) r | d == d' = Bin d' k' a (bin d (k-k') b r)

-- | Get k'th line in direction d in a matrix (for debugging)
line d k Empty = []
line d 0 (Leaf x) = [x]
line d n (Leaf x) = []
line d k (Bin d' k' l r) | d /= d' = pad zero k' (line d k l) ++ line d k r
                         | k < k' = line d k l
                         | k >= k' = line d (k'-k) r

splitAt k d (Bin d' k' l r)
    | d /= d' = let  ~(l1,l2) = splitAt k d l
                     ~(r1,r2) = splitAt k d r
                in (bin d' k' l1 r1,bin d' k' l2 r2)
    | k == k' = (l,r)
    | k < k' = let ~(l1,l2) = splitAt k d l
               in (l1,bin d (k'-k) l2 r)
    | k > k' = let ~(r1,r2) = splitAt (k-k') d r 
               in (bin d k' l r1,r2)
splitAt k d t@(Leaf a) | k > 0 = (t,Empty)
                       | k <= 0 = (Empty,t)
splitAt k d Empty = (Empty,Empty)


minExtent :: PTree a -> Dimension -> Int
minExtent Empty _ = 0
minExtent (Leaf _) _ =  1
minExtent (Bin d k l r) d' | d == d' = k + minExtent r d'
                           | d /= d' = max (minExtent l d') (minExtent r d')

class Validable a where
    validate :: a -> Bool

instance Validable (PTree a) where
    validate Empty = True
    validate (Leaf x) = True
    validate (Bin d k Empty r)  = False
    validate (Bin d k l Empty)  = False
    validate (Bin d k l r) = validate l && validate r && minExtent l d <= k

instance Validable a => Validable (Pair a) where
  validate (a :/: b) = validate a && validate b

  
instance AbelianGroup a => AbelianGroup (PTree a) where
    zero = Empty
    Empty + x = x
    x + Empty = x
    Leaf x + Leaf y = Leaf (x+y)
    Leaf x + Bin d k a b = bin d k (Leaf x + a) b
    (Bin d k a b) + x = bin d k (a' + a) (b' + b)
       where (a',b') = splitAt k d x
       
    

instance AbelianGroup a =>  AbelianGroupZ (PTree a) where
  isZero = isEmpty
  
instance (AbelianGroupZ a, RingP a) => RingP (PTree a) where
   mul p x y = x * y where 
    Empty * x = pure Empty
    x * Empty = pure Empty
    Leaf x * Leaf y = leaf <$> mul p x y
    Leaf x * (Bin YD k a b) = Leaf x * a
    Leaf x * (Bin XD k a b) = bin XD k <$> (Leaf x * a) <*> (Leaf x * b)

    (Bin YD k a b) * x = bin YD k <$> (a *- x)<*> (b *- x)
    (Bin XD k a b) * x = a *- a' + b *- b'
      where (a',b') = splitAt k YD x 
    Empty *- x = pure Empty
    x *- Empty = pure Empty
    Leaf x *- Leaf y =  leaf <$> mul p x y
    (Bin XD k a b) *- Leaf x = a *- Leaf x
    (Bin YD k a b) *- Leaf x = bin YD k <$> (a *- Leaf x) <*> (b *- Leaf x)
    x *- (Bin XD k a b) = bin XD k <$> (x * a) <*> (x * b)
    x *- (Bin YD k a b) = a' * a + b' * b
       where (a',b') = splitAt k XD x 
  
instance (AbelianGroupZ a, Ring a) => Ring (PTree a) where
    Empty * x = Empty
    x * Empty = Empty
    Leaf x * Leaf y = leaf (x*y)
    Leaf x * (Bin YD k a b) = Leaf x * a
    Leaf x * (Bin XD k a b) = bin XD k (Leaf x * a) (Leaf x * b)

    (Bin YD k a b) * x = bin YD k (a *- x) (b *- x)
    (Bin XD k a b) * x = a *- a' + b *- b'
      where (a',b') = splitAt k YD x 
    
Empty *- x = Empty
x *- Empty = Empty
Leaf x *- Leaf y = leaf (x*y)
(Bin XD k a b) *- Leaf x = a *- Leaf x
(Bin YD k a b) *- Leaf x = bin YD k (a *- Leaf x) (b *- Leaf x)
x *- (Bin XD k a b) = bin XD k (x * a) (x * b)
x *- (Bin YD k a b) = a' * a + b' * b
      where (a',b') = splitAt k XD x 

infixl 7 *-

tr Empty = Empty
tr (Leaf x) = Leaf x
tr (Bin d k a b) = Bin (nextDim d) k (tr a) (tr b)

{-
instance Arbitrary a => Arbitrary (PTree a) where
    arbitrary = tSer arbitrary =<< arbitrary
    shrink Empty = []
    shrink (Leaf a) = [Empty]
    shrink x@(Bin d k a b) = [a,b] 
                             
instance Arbitrary Dimension where
    arbitrary = elements [XD, YD]

instance Alternative Gen where
    empty = oneof []
    a <|> b = oneof [a,b]

prop_tr_involution a = tr (tr a) == a

prop_split :: Dimension -> Positive Int -> PTree Int -> Bool
prop_split d (Positive k) a = a == bin d k x y
    where (x,y) = splitAt k d a

prop_pcomm, prop_tr_mul :: PTree Int -> PTree Int -> Bool
prop_pcomm x y = x + y == y + x

prop_passoc, prop_assoc, prop_distrr, prop_distrl :: PTree Int -> PTree Int -> PTree Int -> Bool
prop_assoc x y z = ((x * y) * z) == (x * (y * z))
prop_passoc x y z = ((x + y) + z) == (x + (y + z))
prop_distrr a b c = (a + b) * c == (a * c) + (b * c)
prop_distrl a b c = c * (a + b) == (c * a) + (c * b)
prop_tr_mul a b = tr (a * b) == tr b * tr a

prop_at :: PTree Int -> PTree Int -> Property
prop_at a b = forAll (choose (0,minExtent a YD - 1)) $ \i -> 
               forAll (choose (0,minExtent a XD - 1)) $ \j -> 
                atP j i (a * b) == foldr (+) zero (zipWith (*) (line YD i a) (line XD j b))

tSer a d = choice [pure Empty, Leaf <$> a, 
                do l <- tSer a (nextDim d)                   
                   r <- tSer a (nextDim d)                   
                   when (isEmpty l && isEmpty r) empty
                   return $ bin d (1+minExtent l d) l r]


main = do
  -- print $ head $ failures prop_assoc  
  quickCheck prop_split
  -- quickCheck prop_assoc
-}

lineMatrix :: Dimension -> Int -> a -> PTree (a)

lineMatrix d 0 x = Leaf x
lineMatrix d n x = bin d (2^(n-1)) (lineMatrix d (n-1) x) (lineMatrix d (n-1) x)

columnMatrix = lineMatrix YD
rowMatrix = lineMatrix XD

sqMatrix n x = columnMatrix n x * rowMatrix n x
