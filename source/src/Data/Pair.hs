module Data.Pair where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

infixl 2  :/:

data Pair a = (:/:) {leftOf :: a, rightOf :: a}
  deriving (Show)

instance Functor Pair where
  fmap f (a :/: b) = f a :/: f b

instance Applicative Pair where
  pure a = a :/: a
  (f :/: g) <*> (a :/: b) = f a :/: g b

instance Traversable Pair where
  traverse f (x :/: y) = (:/:) <$> f x <*> f y
  
instance Foldable Pair where
  f `foldMap` (x :/: y) = f x `mappend` f y

