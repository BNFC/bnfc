{-# LANGUAGE GADTs, DataKinds, ScopedTypeVariables, KindSignatures #-}

module Data.Matrix.Quad where

import Prelude ()
import Data.List (splitAt,intercalate)
import Control.Applicative
import Algebra.RingUtils
import Data.Traversable 
import Data.Foldable

data Shape = Bin Shape Shape | Leaf

data Shape' :: Shape -> * where 
  Bin' :: Shape' s -> Shape' s' -> Shape' (Bin s s') 
  Leaf' :: Shape' Leaf

data SomeShape where 
  S :: Shape' s -> SomeShape

data Mat :: Shape -> Shape -> * -> * where
  Quad :: Mat x1 y1 a -> Mat x2 y1 a -> 
          Mat x1 y2 a -> Mat x2 y2 a -> 
          Mat (Bin x1 x2) (Bin y1 y2) a
  Zero :: Mat x y a
  One :: a -> Mat Leaf Leaf a
  Row :: Mat x1 Leaf a -> Mat x2 Leaf a -> Mat (Bin x1 x2) Leaf a
  Col :: Mat Leaf y1 a -> Mat Leaf y2 a -> Mat Leaf (Bin y1 y2) a

row Zero Zero = Zero
row x y = Row x y

col :: Mat Leaf y1 a -> Mat Leaf y2 a -> Mat Leaf (Bin y1 y2) a
col Zero Zero = Zero
col x y = Col x y

quad Zero Zero Zero Zero = Zero
quad a b c d = Quad a b c d

one :: AbelianGroupZ a => a -> Mat Leaf Leaf a
one x | isZero x = Zero
      | otherwise = One x

(.+.) :: AbelianGroupZ a => Mat x y a -> Mat x y a -> Mat x y a
Zero .+. x = x                       
x .+. Zero = x
Quad a b c d .+. Quad a' b' c' d' = quad (a .+. a') (b .+. b') (c .+. c') (d .+. d')
One x .+. One x' = one (x + x')
Row x y .+. Row x' y' = row (x .+. x') (y .+. y')
Col x y .+. Col x' y' = col (x .+. x') (y .+. y')

instance AbelianGroupZ a => AbelianGroup (Mat x y a) where
  (+) = (.+.)
  zero = Zero

mult :: RingP a => Bool -> Mat x y a -> Mat z x a -> Mat z y (Pair a)
mult p a b = a & b where
  infixl 7  &
  (&) :: RingP a => Mat x y a -> Mat z x a -> Mat z y (Pair a)
  Zero & x = Zero
  x & Zero = Zero
  One x & One x' = one (mul p x x')
  One x & Row a b = row (One x & a) (One x & b)
  Col a b & One x = col (a & One x) (b & One x)
  Row a b & Col a' b' = a & a' + b & b'
  Col a b & Row a' b' = quad (a & a') (a & b') (b & a') (b & b')
  Row a b & Quad a' b' c' d' = row (a & a' + b & c') (a & b' + b & d')
  Quad a b c d & Col a' c' = col (a & a' + b & c') (c & a' + d & c')
  Quad a b c d & Quad a' b' c' d' = 
     quad (a & a' + b & c') (a & b' + b & d')
          (c & a' + d & c') (c & b' + d & d')

  x & y = error $ "mult:" ++ intercalate "; " [showR x,showR y]
  
-- a variant of traverse. The constraint prevents to just use traverse.
trav :: AbelianGroupZ a => Mat y x (Pair a) -> Pair (Mat y x a)
trav Zero = pure Zero
trav (Quad a b c d) = quad <$> trav a <*> trav b <*> trav c <*> trav d
trav (One x) = one <$> x
trav (Col a b) = col <$> trav a <*> trav b
trav (Row a b) = row <$> trav a <*> trav b

q0 :: Mat (Bin x x') (Bin y y') a
q0 = Quad Zero Zero Zero Zero

closeDisjointP :: RingP a => Bool -> Mat x x a -> Mat y x (Pair a) -> Mat y y a -> Pair (Mat y x a)
closeDisjointP p l c r = close l c r
  where  close :: RingP a => Mat x x a -> Mat y x (Pair a) -> Mat y y a -> Pair (Mat y x a)
         close l Zero r = Zero :/: Zero
         close Zero x Zero = trav x -- if x = One x', we are in this case
         close (Quad a11 a12 Zero a22) (Quad c11 c12 c21 c22) (Quad b11 b12 Zero b22) = quad <$> x11 <*> x12 <*> x21 <*> x22 
           where x21 = close a22 c21 b11
                 x11 = close a11 (a12 & rightOf x21 + c11) b11
                 x22 = close a22 (leftOf  x21 & b12 + c22) b22
                 x12 = close a11 (a12 & rightOf x22 + leftOf x11 & b12 + c12) b22
         close Zero (Quad c11 c12 c21 c22) (Quad b11 b12 Zero b22) = close q0 (Quad c11 c12 c21 c22) (Quad b11 b12 Zero b22)
         close (Quad a11 a12 Zero a22) (Quad c11 c12 c21 c22) Zero = close (Quad a11 a12 Zero a22) (Quad c11 c12 c21 c22) q0
         close (Quad a11 a12 Zero a22) (Col c1 c2) (Zero) = col <$> x1 <*> x2
           where x2 = close a22 c2 Zero
                 x1 = close a11 (mult p a12 (rightOf x2) + c1) Zero
         close Zero (Row c1 c2) (Quad b11 b12 Zero b22) = row <$> x1 <*> x2
           where x1 = close Zero c1 b11
                 x2 = close Zero (mult p (leftOf x1) b12 + c2) b22
         close a c b = error $ "closeDisjointP:" ++ intercalate "; " [showR a,showR c,showR b]
         (&) :: RingP a => Mat x y a -> Mat z x a -> Mat z y (Pair a)
         (&) = mult p

showR :: Mat x y a -> String
showR Zero = "0"
showR (One _) = "1"
showR (Row a b) = "("++showR a++"-"++showR b++")"
showR (Col a b) = "("++showR a++"|"++showR b++")"
showR (Quad a b c d) = "#("++ intercalate "," [showR a,showR b,showR c,showR d]++")"

mkShape :: Int -> SomeShape
mkShape 1 = S (Bin' Leaf' Leaf')
mkShape 2 = S (Bin' (Bin' Leaf' Leaf') Leaf')
mkShape n = case (mkShape n'1, mkShape n'2) of
  (S x, S y) -> S (Bin' x y)
  where n'1 = n `div` 2
        n'2 = n - n'1 - 1

mkSing :: AbelianGroupZ a => Shape' x -> Shape' y -> a -> Mat x y a
mkSing (Bin' x1 x2) (Bin' y1 y2) a = quad Zero Zero (mkSing x1 y2 a) Zero
mkSing Leaf' Leaf' a = one a
mkSing Leaf' (Bin' y1 y2) a = col Zero (mkSing Leaf' y2 a)
mkSing (Bin' x1 x2) Leaf' a = row (mkSing x1 Leaf' a) Zero

data SomeTri a where                  
  T :: Shape' s -> Pair (Mat s s a) -> SomeTri a
        
type Q a = SomeTri a       
       
-- mkTree :: forall a. AbelianGroupZ a => [Pair a] -> SomeTri a          
-- mkTree xs = case mkShape (length xs) of
--   S s -> T s (mkMat s xs)

quad' a b c d = quad <$> a <*> b <*> c <*> d

mergein :: RingP a => Bool -> SomeTri a -> Pair a -> SomeTri a -> SomeTri a
mergein p (T y a) c (T x b) = T (Bin' y x) (quad' a (closeDisjointP p (leftOf a) c' (rightOf b)) zero b)
  where c' = mkSing x y c
  
        
root' :: AbelianGroup a => Mat x y a -> a
root' Zero = zero
root' (One x) = x
root' (Quad _ a _ _) = root' a
root' (Col a _) = root' a
root' (Row _ a) = root' a


root (T _ (m :/: m')) = root' m + root' m'

single x = T Leaf' (one <$> x)

square2 x = T (Bin' Leaf' Leaf') $ quad' zero (one <$> x) zero zero

square3 p x y = T (Bin' (Bin' Leaf' Leaf') (Leaf')) 
  (quad' (quad' zero (one <$> x) zero zero) (Col <$> (one <$> mul p (leftOf x) (rightOf y)) <*> (one <$> y)) zero zero)
  

sz :: Shape' s -> Int
sz Leaf' = 1
sz (Bin' l r) = sz l + sz r


(|+|) = zipWith (++) 
(-+-) = (++)

lin :: AbelianGroup a => Shape' x -> Shape' y -> Mat x y a -> [[a]]
lin x y Zero = replicate (sz y) $ replicate (sz x) zero
lin _ _ (One x) = [[x]]
lin (Bin' x x') (Bin' y y') (Quad a b c d) = (lin x y a |+| lin x' y b) -+- (lin x y' c |+| lin x' y' d)
lin Leaf' (Bin' y y') (Col a b) = lin Leaf' y a -+- lin Leaf' y' b
lin (Bin' x x') Leaf' (Row a b) = (lin x Leaf' a) |+| (lin x' Leaf' b)

fingerprint (T s (m :/: m')) = map (map (c . not . isZero)) $ lin s s (m + m')
  where 
        c True = 'X'
        c False = ' '
