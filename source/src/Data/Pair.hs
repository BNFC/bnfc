{-# LANGUAGE CPP #-}
module Data.Pair where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

infixl 2  :/:

data Pair a = (:/:) {leftOf :: a, rightOf :: a}
  deriving (Show)

instance Functor Pair where
  fmap f (a :/: b) = f a :/: f b

instance Applicative Pair where
  pure a = a :/: a
  (f :/: g) <*> (a :/: b) = f a :/: g b

