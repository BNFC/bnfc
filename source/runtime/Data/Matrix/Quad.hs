{-# LANGUAGE GADTs, DataKinds, ScopedTypeVariables, KindSignatures, MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}

module Data.Matrix.Quad where

import Prelude ()
import Data.List (splitAt,intercalate)
import Control.Applicative
import Algebra.RingUtils hiding (O,concat)
import Data.Traversable
import Data.Foldable

data Shape = Bin Shape Shape | Leaf

data Shape' :: Shape -> * where
  Bin' :: !Int -> Shape' s -> Shape' s' -> Shape' (Bin s s')
  Leaf' :: Shape' Leaf

-- DEBUG
instance Show (Shape' s) where
    show (Bin' i s1 s2) = "Bin " ++ (show i) ++ " (" ++ show s1 ++ ") (" ++ show s2 ++ ")"
    show Leaf' = "Leaf'"

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

-- DEBUG
instance Show (Mat x y a) where
    show (Quad m1 m2 m3 m4) = "Quad " ++ "("++show m1++") ("++show m2++") ("++show m3++") ("++show m4++")"
    show Zero = "Zero"
    show (One _) = "One <val>"
    show (Row r1 r2) = "Row " ++ "("++show r1++") ("++show r2++")"
    show (Col c1 c2) = "Col " ++ "("++show c1++") ("++show c2++")"

instance Functor (Mat x y) where
    fmap f (Quad a b c d) = quad (fmap f a) (fmap f b) (fmap f c) (fmap f d)
    fmap f Zero = Zero
    fmap f (One a) = One (f a)
    fmap f (Row a b) = row (fmap f a) (fmap f b) 
    fmap f (Col a b) = col (fmap f a) (fmap f b)
  
instance Applicative (Mat x y) where
  Zero <*> _ = Zero
  _ <*> Zero = Zero
  One a <*> One b = One (a b)
  Row a b <*> Row c d = row (a <*> c) (b <*> d)
  Col a b <*> Col c d = col (a <*> c) (b <*> d)
  Quad f g h i <*> Quad a b c d = quad (f <*> a) (g <*> b) (h <*> c) (i <*> d)
  pure a = error "pure: cannot lift a value to Mat x y a"
  
data Vec :: Shape -> * -> * where
  Z :: Vec s a
  O :: a -> Vec Leaf a
  (:!) :: Vec s a -> Vec s' a -> Vec (Bin s s') a

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

mult :: RingP a => Bool -> Mat x y a -> Mat z x a -> Mat z y (Pair a)
mult p a b = a & b where
  infixl 7  &
  (&) :: RingP a => Mat x y a -> Mat z x a -> Mat z y (Pair a)
  Zero & x = Zero
  x & Zero = Zero
  One x & One x' = one (mul p x x')
  One x & Row a b = row (one x & a) (one x & b)
  Col a b & One x = col (a & one x) (b & one x)
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
         close Zero (Quad c11 c12 c21 c22) (Quad b11 b12 Zero b22) = close q0 (quad c11 c12 c21 c22) (quad b11 b12 Zero b22)
         close (Quad a11 a12 Zero a22) (Quad c11 c12 c21 c22) Zero = close (quad a11 a12 Zero a22) (quad c11 c12 c21 c22) q0
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
  T :: Shape' s -> Pair (Mat s s a) -> SomeTri a

mkUpDiag :: AbelianGroupZ a => [a] -> Shape' s -> Mat s s a
mkUpDiag [] Leaf' = Zero
mkUpDiag xs (Bin' _ s s') = quad (mkUpDiag a s) (mkSing s' s c) Zero (mkUpDiag b s')
  where (a,c:b) = splitAt (sz' s - 1) xs

close :: RingP a => Bool -> Mat s s (Pair a) -> Pair (Mat s s a)
close p Zero = zero
close p (One x) = one <$> x
close p (Quad a11 a12 Zero a22) = quad' x11 (closeDisjointP p (leftOf x11) a12 (rightOf x22)) zero x22
 where x11 = close (not p) a11
       x22 = close (not p) a22

mkTree :: RingP a => [Pair a] -> SomeTri a
mkTree xs = case mkShape (length xs) of
  S s -> T s (close True $ mkUpDiag xs s)

quad' a b c d = quad <$> a <*> b <*> c <*> d

mergein :: RingP a => Bool -> SomeTri a -> Pair a -> SomeTri a -> SomeTri a
mergein p (T y a) c (T x b) = T (bin' y x) (quad' a (closeDisjointP p (leftOf a) c' (rightOf b)) zero b)
  where c' = mkSing x y c

data ChopFirst x x' where
  Stop :: ChopFirst (Bin Leaf x) x
  Continue :: ChopFirst x x' -> ChopFirst (Bin x x0) (Bin x' x0)

-- Given two matrices with the same y component, create one with the combined x
-- component as its shape. 
mkRow :: AbelianGroupZ a => Mat x y a -> Mat x' y a -> Mat (Bin x x') y a
mkRow Zero Zero      = Zero
mkRow Zero (One a)   = row Zero (one a)
mkRow Zero (Col a b) = quad Zero a Zero b
mkRow Zero (Row a b) = row Zero (row a b)
mkRow Zero (Quad a b c d) = quad Zero (mkRow a b) Zero (mkRow c d)
mkRow (One a) Zero      = row (one a) Zero
mkRow (One a) (One b)   = row (one a) (one b)
mkRow (One a) (Row b c) = row (one a) (row b c)
mkRow (Row a b) Zero    = row (row a b) Zero
mkRow (Row a b) (One c) = row (row a b) (one c) 
mkRow (Row a b) (Row c d) = row (row a b) (row c d)
mkRow (Col a b) Zero           = quad a Zero b Zero
mkRow (Col a b) (Col c d)      = quad a c b d
mkRow (Col a b) (Quad c d e f) = quad a (mkRow c d) b (mkRow e f)
mkRow (Quad a b c d) Zero      = quad (mkRow a b) Zero (mkRow c d) Zero
mkRow (Quad a b c d) (Col e f) = quad (mkRow a b) e (mkRow c d) f
mkRow (Quad a b c d) (Quad e f g h) = quad (mkRow a b) (mkRow e f) (mkRow c d) (mkRow g h)
mkRow m1 m2 = error $ "mkRow: " ++ show m1 ++ show m2

-- Chop off the topmost row in a matrix
chopFirstRow :: AbelianGroupZ a => ChopFirst y y' -> Mat x y a -> (Mat x y' a, Mat x Leaf a)
chopFirstRow _ Zero              = (Zero, Zero)
chopFirstRow Stop (Col a b)      = (b, a)
chopFirstRow Stop (Quad a b c d) = (mkRow c d, row a b)
chopFirstRow (Continue ch) (Col a b) = first (\a' -> col a' b) $ chopFirstRow ch a
chopFirstRow (Continue ch) (Quad a b c d) =
    let (a', topleft)  = chopFirstRow ch a
        (b', topright) = chopFirstRow ch b
    in (quad a' b' c d, row topleft topright)

-- Chop off the first col (which we discard) and the first row (which we return)
-- of a matrix.
chopFirst :: RingP a => ChopFirst x x' -> Mat x x a  -> (Mat x' Leaf a,Mat x' x' a)
chopFirst _ Zero = (Zero,Zero)
chopFirst Stop (Quad a b c d) = (b,d)
chopFirst (Continue q) (Quad a b c d) =
  let  (e, a') = chopFirst q a
       (b',f)  = chopFirstRow q b
  in (row e f,quad a' b' zero d)
chopFirst k ms = error $ "chopFirst: " ++ show ms  -- DEBUG

chopShape :: Shape' x -> (forall x'. ChopFirst x x' -> Shape' x' -> k) -> k
chopShape Leaf' k = error "chopShape: can't chop!"
chopShape (Bin' _ Leaf' y) k = k Stop y
chopShape (Bin' _ y1 y2) k = chopShape y1 $ \q y1' -> k (Continue q) (bin' y1' y2) 

-- | Extend a single row matrix along the y axis, and discard all but one element
mkLast :: RingP a => Shape' y -> Mat x Leaf a -> Mat x y a
mkLast Leaf' m = m
mkLast (Bin' _ _ y) Zero = zero
mkLast (Bin' _ _ y) (One a) = col zero (mkLast y $ one a)
mkLast (Bin' _ _ y) (Row a b) = quad zero zero (mkLast y a) zero

-- | Merge two upper triangular matricies without a middle element.
merge :: RingP a => Bool -> SomeTri a -> SomeTri a -> SomeTri a
merge p (T s (Zero :/: Zero)) x = x
merge p x (T s (Zero :/: Zero)) = x
-- Values should be ABOVE the diagonal, hence 1x1 can be discarded
-- merge p (T Leaf' _zero) x = x
-- merge p x (T Leaf' _zero) = x
-- General case
merge p (T y l) (T x r) = chopShape x $ \chopper x' ->
    let (rTopL, rL') = chopFirst chopper (leftOf r)
        (rTopR, rR') = chopFirst chopper (rightOf r)
        cdp = closeDisjointP p (leftOf l) (mkLast y $ sequenceA (rTopL :/: rTopR)) rR'
    in T (bin' y x') (quad' l cdp zero (rL' :/: rR'))

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

unsparse :: AbelianGroup a => [(Int,a)] -> Int -> [a]
unsparse [] n = replicate n zero
unsparse ((ix,v):rs) n = replicate ix zero ++ v : unsparse (map (first (\x -> x - ix - 1)) rs) (n - ix - 1)

denseContents :: AbelianGroup a => Shape' x -> Vec x a -> [a]
denseContents s v = unsparse (contents s v) (sz' s)

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
x <||> y = (second High <$> y) <|> (second Low <$> x)

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
results (T s (m :/: m')) = [(fromPath s x,a,fromPath s y) | (x,a,y) <- results' (m+m') (leftMost s)]

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

root (T _ (m :/: m')) = root' m + root' m'

single x = T Leaf' (one <$> x)

square2 x = T (bin' Leaf' Leaf') $ quad' zero (one <$> x) zero zero

square3 p x y = T (bin' (bin' Leaf' Leaf') (Leaf'))
  (quad' (quad' zero (one <$> x) zero zero) (col <$> (one <$> mul p (leftOf x) (rightOf y)) <*> (one <$> y)) zero zero)


sz' :: Shape' s -> Int
sz' Leaf' = 1
sz' (Bin' x l r) = x -- sz' l + sz' r

lin :: AbelianGroup a => Shape' x -> Shape' y -> Mat x y a -> [[a]]
lin s1 s2 m = fmap (denseContents s1) $ denseContents s2 $ lin' m

sparse :: AbelianGroup a => Shape' x -> Shape' y -> Mat x y a -> [(Int,Int,a)]
sparse x y Zero = []
sparse _ _ (One x) = [(0,0,x)]
sparse (Bin' _ x x') (Bin' _ y y') (Quad a b c d) = sparse x y a ++ shiftX x (sparse x' y b) ++ shiftY y (sparse x y' c) ++ shiftX x (shiftY y(sparse x' y' d))
sparse Leaf' (Bin' _ y y') (Col a b) = sparse Leaf' y a ++ shiftY y (sparse Leaf' y' b)
sparse (Bin' _ x x') Leaf' (Row a b) = sparse x Leaf' a ++ shiftX x (sparse x' Leaf' b)

shiftX x0 as = [(x+sz' x0,y,a) | (x,y,a) <- as]
shiftY y0 as = [(x,y+sz' y0,a) | (x,y,a) <- as]

fingerprint (T s (m :/: m')) = zipWith (zipWith c) (lin s s m) (lin s s m')
  where c x y = case (isZero x,isZero y) of
                     (True  , True) -> ' '
                     (True  , False) -> '>'
                     (False , True) -> '<'
                     (False , False) -> 'X'

scatterplot (T s (m :/: m')) = concat [show x ++ " " ++ show y ++ "\n" | (x,y,_) <- sparse s s m ++ sparse s s m']

