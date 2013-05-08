module CYK where

import Data.Monoid
import Data.Array.IArray
import Data.List
import Control.Monad

import Prelude ()
import Prelude.YAP
import CNF
import Examples
import Chart


data Chart a =
    Single a |
    Merge {
      mergeSize :: Int,
      here   :: (Array (Int,Int) a),
      left   :: Chart a,
      right  :: Chart a
    } -- invariant : size = size left + size right
    deriving Show

size (Single _) = 1
size (Merge s _ _ _) = s


access start end w | start < 0        =  error "start<0"
                   | start >= size w  =  error "start>=n"
                   | end <= 0         =  error "end<=0"
                   | end > size w     =  error "end>n"
                   | start >= end     =  error "start>=end"
                   | otherwise        =  access' start end w


access' 0 1 (Single x) = x
access' start end (Merge n h l r)
    | end   <= size l  =  access start end l
    | start >= size l  =  access (start - size l) (end - size l) r
    | otherwise        =  h ! (start,n-end)

showW w = forM_ [1..n] $ \end -> do
            forM_ [0..end-1] $ \start ->
                putStr (show (access start end w) ++ " ")
            putStrLn ""
     where n = size w


merge l r = result
    where
      x i j = access i j result
      result = Merge n h l r
      n = size l + size r
      h = array ((0,0),(size l-1, size r-1))
                [((start,n-end), nub [c | k <- [start+1..end-1], c <- x start k * x k end])
                 | start <- [0..size l-1],
                   end   <- [size l+1..n]
                ]


instance IsChart Chart where
   single nt = Single nt
   (<>) = merge
