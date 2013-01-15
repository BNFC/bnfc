{-# LANGUAGE GADTs, DataKinds, ScopedTypeVariables, KindSignatures #-}

module Data.Matrix.Quad where

import Prelude ()
import Data.List (splitAt,intercalate)
import Control.Applicative
import Algebra.RingUtils -- hiding (O)
import Data.Traversable 
import Data.Foldable

data Shape = Bin Shape Shape | Leaf

data Shape' :: Shape -> * where 
  Bin' :: !Int -> Shape' s -> Shape' s' -> Shape' (Bin s s') 
  Leaf' :: Shape' Leaf

data SomeShape where 
  S :: Shape' s -> SomeShape

data Mat :: Shape -> Shape -> * -> * where
  Quad :: !(Mat x1 y1 a) -> !(Mat x2 y1 a) -> 
          !(Mat x1 y2 a) -> !(Mat x2 y2 a) -> 
          Mat (Bin x1 x2) (Bin y1 y2) a
  Zero :: Mat x y a
  One :: !a -> Mat Leaf Leaf a
  Row :: Mat x1 Leaf a -> Mat x2 Leaf a -> Mat (Bin x1 x2) Leaf a
  Col :: Mat Leaf y1 a -> Mat Leaf y2 a -> Mat Leaf (Bin y1 y2) a

showR :: Mat x y a -> String
showR Zero = "0"
showR (One _) = "1"
showR (Row a b) = "("++showR a++"-"++showR b++")"
showR (Col a b) = "("++showR a++"|"++showR b++")"
showR (Quad a b c d) = "#("++ intercalate "," [showR a,showR b,showR c,showR d]++")"


data Vec :: Shape -> * -> * where
  Z :: Vec s a
  O :: a -> Vec Leaf a
  (:!) :: Vec s a -> Vec s' a -> Vec (Bin s s') a


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

mult :: RingU a => Mat x y a -> Mat z x a ->  (Mat x y a, Mat z y a, Mat z x a)
mult a b = a & b where
  infixl 7  &
  (&) :: RingU a => Mat x y a -> Mat z x a -> (Mat x y a, Mat z y a, Mat z x a)
  Zero & y = (Zero,Zero,y)
  x & Zero = (x,Zero,Zero)
  One x & One y = (one x',one a, one y')
    where (x',a,y') = mul x y
  x@(One _) & Row a b = runI $ do
    let (x,b',b) = x & b
    let (x,a',a) = x & a
    return (x,row a' b', row a b)
  Col a b & x@(One _) = runI $ do 
    let (a,a',x) = a & x
    let (b,b',x) = b & x
    return (col a b,col a' b',x)
  Row a b & Col a' b' = runI $ do
    let (a,x,a') = a & a'
    let (b,y,b') = b & b'
    return (row a b,x + y,col a' b')

  Col a b & Row a' c' = runI $ do 
     let (b,w ,a') = b & a'
     let (a,v, b') = a & b'
     let (b,x ,b') = b & b'
     let (a,u, a') = a & a'
     return (col a b, quad u v w x, row a' c')

  Row a b & Quad a' b' c' d' = runI $ do 
     let (a,v, b') = a & b'
     let (b,v',d') = b & d'
     let (a,u, a') = a & a'
     let (b,u',c') = b & c'
     return (row a b, row (u+u') (v+v'), quad a' b' c' d')

  Quad a b c d & Col a' c' = runI $ do 
     let (c,w ,a') = c & a'
     let (d,w',c') = d & c'
     let (a,u, a') = a & a'
     let (b,u',c') = b & c'
     return (quad a b c d, col (u+u') (w+w'), col a' c')

  Quad a b c d & Quad a' b' c' d' = runI $ do 
     let (c,w ,a') = c & a'
     let (d,w',c') = d & c'
     let (a,v, b') = a & b'
     let (b,v',d') = b & d'
     let (c,x ,b') = c & b'
     let (d,x',d') = d & d'
     let (a,u, a') = a & a'
     let (b,u',c') = b & c'
     return (quad a b c d, quad (u+u') (v+v') (w+w') (x+x'), quad a' b' c' d')

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

data I a = I {runI :: a}
instance Monad I where
  return = I
  x >>= f = f (runI x)

closeDisjointU :: RingU a => Mat x x a -> Mat y x a -> Mat y y a -> (Mat x x a, Mat y x a, Mat y y a)
closeDisjointU l c r = close l c r
  where  close :: RingU a => Mat x x a -> Mat y x a -> Mat y y a -> (Mat x x a, Mat y x a, Mat y y a)
         close l Zero r = (l,Zero,r)
         close Zero x Zero = (Zero,x,Zero)
         close (Quad a11 a12 Zero a22) (Quad c11 c12 c21 c22) (Quad b11 b12 Zero b22) = runI $ do
           let (a22,x21,b11)  = close a22 c21 b11
           let (a12,y11,x21)  = a12 `mult` x21
           let (x21,y22,b12) = x21 `mult` b12
           let (a11,x11,b11) = close a11 (y11 + c11) b11
           let (a22,x22,b22) = close a22 (y22 + c22) b22
           let (a12,y12,x22) = a12 `mult` x22
           let (x11,z12,b12) = x11 `mult` b12
           let (a11,x12,b22) = close a11 (y12 + z12 + c12) b22
           return (quad a11 a12 Zero a22, Quad x11 x12 x21 x22,quad b11 b12 Zero b22)
         close Zero (Quad c11 c12 c21 c22) (Quad b11 b12 Zero b22) = close q0 (Quad c11 c12 c21 c22) (Quad b11 b12 Zero b22)
         close (Quad a11 a12 Zero a22) (Quad c11 c12 c21 c22) Zero = close (Quad a11 a12 Zero a22) (Quad c11 c12 c21 c22) q0
         close (Quad a11 a12 Zero a22) (Col c11 c21) Zero = runI $ do
           let (a22,x21,Zero)  = close a22 c21 Zero
           let (a12,y11,x21) = a12 `mult` x21
           let (a11,x11,Zero) = close a11 (y11 + c11) Zero
           return (quad a11 a12 Zero a22, Col x11 x21,Zero)
         close Zero (Row c21 c22) (Quad b11 b12 Zero b22) = runI $ do
           let (Zero,x21,b11) = close Zero c21 b11
           let (x21,y22,b12) = x21 `mult` b12
           let (Zero,x22,b22) = close Zero (y22 + c22) b22
           return (Zero, Row x21 x22,quad b11 b12 Zero b22)
         close a c b = error $ "closeDisjointP:" ++ intercalate "; " [showR a,showR c,showR b]
bin' :: Shape' s -> Shape' s' -> Shape' (Bin s s')
bin' s s' = Bin' (sz' s + sz' s') s s'

mkShape :: Int -> SomeShape
mkShape 1 = S (bin' Leaf' Leaf')
mkShape 2 = S (bin' (bin' Leaf' Leaf') Leaf')
mkShape n = case (mkShape n'1, mkShape n'2) of
  (S x, S y) -> S (bin' x y)
  where n'1 = n `div` 2
        n'2 = n - n'1 - 1

mkSing :: AbelianGroupZ a => Shape' x -> Shape' y -> a -> Mat x y a
mkSing (Bin' _ x1 x2) (Bin' _ y1 y2) a = quad Zero Zero (mkSing x1 y2 a) Zero
mkSing Leaf' Leaf' a = one a
mkSing Leaf' (Bin' _ y1 y2) a = col Zero (mkSing Leaf' y2 a)
mkSing (Bin' _ x1 x2) Leaf' a = row (mkSing x1 Leaf' a) Zero

data SomeTri a where                  
  T :: Shape' s -> Mat s s a -> SomeTri a
        
type Q a = SomeTri a       
       
mkUpDiag :: AbelianGroupZ a => [a] -> Shape' s -> Mat s s a
mkUpDiag [] Leaf' = Zero
mkUpDiag xs (Bin' _ s s') = Quad (mkUpDiag a s) (mkSing s' s c) Zero (mkUpDiag b s')
  where (a,c:b) = splitAt (sz' s - 1) xs

close :: RingU a => Mat s s a -> Mat s s a
close Zero = zero
close (One x) = one x
close (Quad a11 a12 Zero a22) = runI $ do
 let (a11,a12,a22) = closeDisjointU (close a11) a12 (close a22)
 return $ Quad a11 a12 Zero a22

mkTree :: RingU a => [a] -> SomeTri a          
mkTree xs = case mkShape (length xs) of
  S s -> T s (close $ mkUpDiag xs s)

quad' a b c d = quad <$> a <*> b <*> c <*> d

{-mergein :: RingU a => Bool -> SomeTri a -> Pair a -> SomeTri a -> SomeTri a
mergein p (T y a) c (T x b) = T (bin' y x) (quad' a (closeDisjointP p (leftOf a) c' (rightOf b)) zero b)
  where c' = mkSing x y c
  -}

-- | A variant of zipWith on vectors
zw :: (AbelianGroup a, AbelianGroup b) => (a -> b -> c) -> Vec y a -> Vec y b -> Vec y c
zw f Z Z = Z
zw f Z (a :! b) = zw f (Z :! Z) (a :! b)
zw f (a :! b) Z = zw f (a :! b) (Z :! Z)
zw f Z (O x) = O $ f zero x
zw f (O x) Z = O $ f x zero
zw f (O x) (O y) = O (f x y)
zw f (a :! b) (a' :! b') = zw f a a' :! zw f b b'

-- | Lookup in a vector
lk :: AbelianGroup a => Int -> Shape' x -> Vec x a -> a
lk n _ Z = zero
lk 0 Leaf' (O x) = x
lk i (Bin' _ s s') (x :! x') 
  | i < sz' s  = lk i s x
  | otherwise = lk (i - sz' s) s' x'

-- | Linearize a matrix
lin' :: AbelianGroup a => Mat x y a -> Vec y (Vec x a)
lin' Zero = Z
lin' (One a) = O (O a)
lin' (Row a b) = zw (:!) (lin' a) (lin' b)
lin' (Col a b) = lin' a :! lin' b
lin' (Quad a b c d) = zw (:!) (lin' a) (lin' b) :!zw (:!) (lin' c) (lin' d)

-- | Contents of a vector
contents :: Shape' x -> Vec x a -> [(Int,a)]
contents s Z = [] 
contents s (O a) = [(0,a)]
contents (Bin' _ s s') (xs :! xs') = contents s xs ++ map (first (+sz' s)) (contents s' xs')


first f (a,b) = (f a,b)
second f (a,b) = (a,f b)

instance AbelianGroup a => AbelianGroup (Vec x a) where
  zero = Z
  (+) = zw (+)

data Path :: Shape -> * where
  Here :: Path Leaf
  Low  :: Path s -> Path (Bin s s') 
  High :: Path s -> Path (Bin s' s) 

(<||>) :: Maybe (a,Path x) -> Maybe (a,Path x') -> Maybe (a,Path (Bin x x'))
x <||> y =  (second High <$> y) <|> (second Low <$> x) 

-- | What is, and where is the rightmost non-zero element on a given
-- line of the matrix?
rightmostOnLine :: Path y -> Mat x y a -> Maybe (a,Path x)
rightmostOnLine _ Zero = Nothing
rightmostOnLine Here (One x) = Just (x,Here)
rightmostOnLine Here (Row a b)    = rightmostOnLine Here a <||> rightmostOnLine Here b
rightmostOnLine (Low p) (Col a b) = rightmostOnLine p a
rightmostOnLine (High p) (Col a b) = rightmostOnLine p b
rightmostOnLine (Low p) (Quad a b _ _) = rightmostOnLine p a <||> rightmostOnLine p b
rightmostOnLine (High p) (Quad _ _ a b) = rightmostOnLine p a <||> rightmostOnLine p b

-- | Is this the rightmost path?
isRightmost :: Path x -> Bool
isRightmost (Low _) = False
isRightmost (Here) = True
isRightmost (High x) = isRightmost x

results' :: AbelianGroup a => Mat y y a -> Path y -> [(Path y, a, Path y)]
results' m y | isRightmost y = []
             | otherwise = (y,a,x) : results' m x
  where Just (a,x) = rightmostOnLine y m

results :: AbelianGroupZ a => SomeTri a -> [(Int, a, Int)]
results (T s m) = [(fromPath s x,a,fromPath s y) | (x,a,y) <- results' m (leftMost s)]

leftMost :: Shape' s -> Path s
leftMost Leaf' = Here
leftMost (Bin' _ s _) = Low $ leftMost s

fromPath :: Shape' y -> Path y -> Int
fromPath _ Here =  0
fromPath (Bin' _ s s') (Low x) = fromPath s x
fromPath (Bin' _ s s') (High x) = sz' s + fromPath s' x 

                               
root' :: AbelianGroup a => Mat x y a -> a
root' Zero = zero
root' (One x) = x
root' (Quad _ a _ _) = root' a
root' (Col a _) = root' a
root' (Row _ a) = root' a

root (T _ m) = root' m

single x = T Leaf' (one x)

square2 x = T (bin' Leaf' Leaf') $ quad zero (one x) zero zero


sz' :: Shape' s -> Int
sz' Leaf' = 1
sz' (Bin' x l r) = x -- sz' l + sz' r


(|+|) = zipWith (++) 
(-+-) = (++)

-- TODO: reimplement using lin'
lin :: AbelianGroup a => Shape' x -> Shape' y -> Mat x y a -> [[a]]
lin x y Zero = replicate (sz' y) $ replicate (sz' x) zero
lin _ _ (One x) = [[x]]
lin (Bin' _ x x') (Bin' _ y y') (Quad a b c d) = (lin x y a |+| lin x' y b) -+- (lin x y' c |+| lin x' y' d)
lin Leaf' (Bin' _ y y') (Col a b) = lin Leaf' y a -+- lin Leaf' y' b
lin (Bin' _ x x') Leaf' (Row a b) = (lin x Leaf' a) |+| (lin x' Leaf' b)

fingerprint :: AbelianGroupZ a => SomeTri a -> [[Char]]
fingerprint (T s m) = map (map c) (lin s s m)
  where c x = case (isZero x) of
                     (True  ) -> ' '
                     (False ) -> 'X'



