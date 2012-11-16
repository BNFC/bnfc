{-# LANGUAGE GADTs, DataKinds, ScopedTypeVariables, KindSignatures #-}

module Data.Matrix.Quad where

import Prelude ()
import Data.List (splitAt)
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

infixl 7  .*.

mult :: RingP a => Bool -> Mat x y a -> Mat z x a -> Mat z y (Pair a)
mult = undefined

(.*.) :: Ring a => Mat x y a -> Mat z x a -> Mat z y a

Zero .*. x = Zero
x .*. Zero = Zero
  
One x .*. One x' = one (x * x')
One x .*. Row a b = row (One x .*. a) (One x .*. b)
Col a b .*. One x = col (a .*. One x) (b .*. One x)
Row a b .*. Col a' b' = a .*. a' + b .*. b'
Row a b .*. Quad a' b' c' d' = row (a .*. a' + b .*. c') (a .*. b' + b .*. d')
Quad a b c d .*. Col a' c' = col (a .*. a' + b .*. c') (c .*. a' + d .*. c')
Quad a b c d .*. Quad a' b' c' d' = 
   quad (a .*. a' + b .*. c') (a .*. b' + b .*. d')
        (c .*. a' + d .*. c') (c .*. b' + d .*. d')

-- a variant of traverse. The constraint prevents to just use traverse.
trav :: AbelianGroupZ a => Mat y x (Pair a) -> Pair (Mat y x a)
trav Zero = pure Zero
trav (Quad a b c d) = quad <$> trav a <*> trav b <*> trav c <*> trav d
trav (One x) = one <$> x
trav (Col a b) = col <$> trav a <*> trav b
trav (Row a b) = row <$> trav a <*> trav b

closeDisjointP :: forall a x y. RingP a => Bool -> Mat x x a -> Mat y x (Pair a) -> Mat y y a -> Pair (Mat y x a)
closeDisjointP p l Zero r = Zero :/: Zero
closeDisjointP p Zero x Zero = trav x -- if x = One x', we are in this case
closeDisjointP p (Quad a11 a12 Zero a22) (Quad c11 c12 c21 c22) (Quad b11 b12 Zero b22) = quad <$> x11 <*> x12 <*> x21 <*> x22 
  where x21 = closeDisjointP p a22 c21 b11
        x11 = closeDisjointP p a11 (a12 & rightOf x21 + c11) b11
        x22 = closeDisjointP p a22 (leftOf x21 & b12 + c22) b22
        x12 = closeDisjointP p a11 (a12 & rightOf x22 + leftOf x11 & b12 + c12) b22
        infixl 7  &
        (&) :: forall x y z. Mat x y a -> Mat z x a -> Mat z y (Pair a)
        (&) = mult p
closeDisjointP p (Quad a11 a12 Zero a22) (Col c1 c2) (Zero) = col <$> x1 <*> x2
  where x2 = closeDisjointP p a22 c2 Zero
        x1 = closeDisjointP p a11 (mult p a12 (rightOf x2) + c1) Zero
closeDisjointP p Zero (Row c1 c2) (Quad b11 b12 Zero b22) = row <$> x1 <*> x2
  where x1 = closeDisjointP p Zero c1 b11
        x2 = closeDisjointP p Zero (mult p (leftOf x1) b12 + c2) b22

mkShape :: Int -> SomeShape
mkShape 1 = S (Bin' Leaf' Leaf')
mkShape 2 = S (Bin' (Bin' Leaf' Leaf') Leaf')
mkShape n = case (mkShape n'1, mkShape n'2) of
  (S x, S y) -> S (Bin' x y)
  where n'1 = n `div` 2
        n'2 = n - n'1 - 1

sz :: Shape' s -> Int
sz Leaf' = 0
sz (Bin' l r) = 1 + sz l + sz r

mkSing :: AbelianGroupZ a => Shape' x -> Shape' y -> a -> Mat x y a
mkSing (Bin' x1 x2) (Bin' y1 y2) a = quad Zero Zero (mkSing x1 y2 a) Zero
mkSing Leaf' Leaf' a = one a
mkSing Leaf' (Bin' y1 y2) a = col Zero (mkSing Leaf' y2 a)
mkSing (Bin' x1 x2) Leaf' a = row (mkSing x1 Leaf' a) Zero

{-
  
closeDisjoint :: Ring a => Mat x x a -> Mat y x a -> Mat y y a -> Mat y x a
closeDisjoint l Zero r = Zero
closeDisjoint Zero x Zero = x -- if x = One x', we are in this case
closeDisjoint (Quad a11 a12 Zero a22) (Quad c11 c12 c21 c22) (Quad b11 b12 Zero b22) = quad x11 x12 x21 x22 
  where x21 = closeDisjoint a22 c21 b11
        x11 = closeDisjoint a11 (a12 .*. x21 + c11) b11
        x22 = closeDisjoint a22 (x21 .*. b12 + c22) b22
        x12 = closeDisjoint a11 (a12 .*. x22 + x11 .*. b12 + c12) b22
closeDisjoint (Quad a11 a12 Zero a22) (Col c1 c2) (Zero) = col x1 x2
  where x2 = closeDisjoint a22 c2 Zero
        x1 = closeDisjoint a11 (a12 .*. x2 + c1) Zero
closeDisjoint Zero (Row c1 c2) (Quad b11 b12 Zero b22) = row x1 x2
  where x1 = closeDisjoint Zero c1 b11
        x2 = closeDisjoint Zero (x1 .*. b12 + c2) b22


close :: Ring a => Mat x x a -> Mat x x a
close (One x) = One x
close (Quad a c Zero b) = quad a (closeDisjoint a c b) Zero b

mkMat :: AbelianGroupZ a => Shape' s -> [a] -> Mat s s a
mkMat Leaf' [] = Zero
mkMat (Bin' sl sr) xs = quad (mkMat sl l) (mkSing sr sl x) Zero (mkMat sr r)
  where (l,x:r) = splitAt (sz sl) xs
-}   
                  
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

square2 x = T (Bin' Leaf' Leaf') $ quad' zero zero (one <$> x) zero

square3 p x y = T (Bin' (Bin' Leaf' Leaf') (Leaf')) 
  (quad' (quad' zero (one <$> x) zero zero) (Col <$> (one <$> mul p (leftOf x) (rightOf y)) <*> (one <$> y)) zero zero)
  

lin :: AbelianGroup a => Shape' x -> Shape' y -> Mat x y a -> [[a]]
lin x y Zero = replicate (sz y) $ replicate (sz x) zero
lin _ _ (One x) = [[x]]
lin (Bin' x x') (Bin' y y') (Quad a b c d) = zipWith (++) (lin x y a ++ lin x' y b) (lin x y' c ++ lin x' y' d)
lin Leaf' (Bin' y y') (Col a b) = (lin Leaf' y a ++ lin Leaf' y' b)
lin (Bin' x x') Leaf' (Row a b) = zipWith (++) (lin x Leaf' a) (lin x' Leaf' b)

genXPM xs@(h:_) = unlines $
  ["! XPM2",
   -- <width/cols> <height/rows> <colors> <char on pixel>
   show width ++ " " ++ show height ++ " 2 1",
   "X c green",
   "  c black"
   ] ++ xs
  where width = length h
        height = length xs

fingerprint (T s (m :/: m')) = map (map (c.isZero)) $ lin s s (m + m')
  where 
        c True = 'X'
        c False = ' '

{-
instance Matrix SomeTri where
  countRows (T s _) = sz s
  countColumns = countRows
  at i j (T _ Zero) = zero
  at i j (T _ (One x)) = x
  at i j (T (Bin s1 s2)) = 
-}