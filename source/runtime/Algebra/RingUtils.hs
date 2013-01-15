{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Algebra.RingUtils
  ( module Prelude
  , AbelianGroup(..)
  , AbelianGroupZ(..)
  , Ring(..)
  , RingU(..)
  , Pair(..) -- select, onlyLeft, onlyRight
--  , O(..)
  , sum
  , module Data.Pair
  )
 where

import qualified Prelude as P
import Prelude hiding ( (+), (*), splitAt, sum )
import Control.Applicative
import Data.Pair

class AbelianGroup a where
    zero :: a
    (+)  :: a -> a -> a

instance AbelianGroup Int where
    zero = 0
    (+)  = (P.+)

class AbelianGroup a => AbelianGroupZ a where
    isZero :: a -> Bool

instance AbelianGroupZ Int where
    isZero x = x == 0

class AbelianGroupZ a => Ring a where
    (*) :: a -> a -> a    

class (AbelianGroupZ a) => RingU a where
    mul :: a -> a -> (a,a,a)

newtype O f g a = O {fromO :: f (g a)}
  deriving (AbelianGroup, AbelianGroupZ, Show)
           
instance (Functor f,Functor g) => Functor (O f g) where
   fmap f (O x) = O (fmap (fmap f) x)

instance AbelianGroup a => AbelianGroup (Pair a) where
  zero = (zero:/:zero)
  (a:/:b) + (x:/:y) = (a+x) :/: (b+y)

instance AbelianGroupZ a => AbelianGroupZ (Pair a) where
  isZero (a:/:b)  = isZero a && isZero b

instance Ring Int where
    (*)  = (P.*)

infixl 7  *
infixl 6  +

sum :: AbelianGroup a => [a] -> a
sum = foldr (+) zero

instance AbelianGroup Bool where
  zero = False
  (+) = (||)