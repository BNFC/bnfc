{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, GADTs, RankNTypes #-}

module Matrix (
  Bounding (..),
  MT(..),
  mdepth,
  ) where

import Control.Applicative
import Prelude ()
import Tree2
import Matrices
import Zero



newtype MT a = MT (Bounding (PTree a))
  deriving (Eq,AbelianGroupZ,AbelianGroup,Show,RingP,Functor)               

mdepth (MT (Bounding _ x)) = depth x

data Bounding a = Bounding {extent :: Extent, contents :: a}
  deriving (Show, Functor) -- LANGUAGE DeriveFunctor


instance AbelianGroup a => AbelianGroup (Bounding a) where
  ~(Bounding x c) + ~(Bounding _ c') = Bounding x -- trust the user to match sizes. Otherwise we have a very strict function.
                                 (c + c')
  zero = Bounding (error "no extent for zero-matrix") zero

instance (AbelianGroupZ a, Ring a) => Ring (Bounding a) where
  Bounding (_,y) a * Bounding (x,_) b = Bounding (x,y) (a * b)
  
instance (AbelianGroupZ a, RingP a) => RingP (Bounding a) where
  mul p (Bounding (_,y) a) (Bounding (x,_) b) = Bounding (x,y) <$> (mul p a b)


instance AbelianGroupZ a => AbelianGroupZ (Bounding a) where
  isZero (Bounding _ z) = isZero z

instance Matrix MT where
  at i j (MT (Bounding _ x)) = atP i j x
  extent (MT (Bounding x _)) = x
  singleton x = MT (Bounding (1,1) (leaf x))
  glue d (MT (Bounding x1 c1)) (MT (Bounding x2 c2)) = MT (Bounding (glueExt d x1 x2) $ bin d (ext d x1) c1 c2)
  split d k (MT (Bounding x c)) = (MT,MT) $$$ ((Bounding,Bounding) $$$ (splitExt d k x) $$$ (splitAt k d c))
  zeroMatrix x y = MT (Bounding (x,y) Empty)
  
instance (Eq a, AbelianGroupZ a) => Eq (Bounding a) where
  Bounding x1 c1 == Bounding x2 c2 = x1 == x2 && c1 == c2
  
printM :: (AbelianGroup a,Show a) => MT a -> IO ()
printM (MT (Bounding x c)) = do
  print x
  printT c 

{-
instance Validable (MT a) where
    validate (MT (Bounding (xs,ys) t)) = validate t && minExtent t XD <= xs
                                                    && minExtent t YD <= ys

-}