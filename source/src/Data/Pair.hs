module Data.Pair where

import Control.Applicative

infixl 2  :/:

data Pair a = (:/:) {leftOf :: a, rightOf :: a}
  deriving (Show)

instance Functor Pair where
  fmap f (a :/: b) = f a :/: f b

instance Applicative Pair where
  pure a = a :/: a
  (f :/: g) <*> (a :/: b) = f a :/: g b

