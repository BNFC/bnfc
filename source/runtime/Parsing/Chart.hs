module Chart where

import Data.Array
import Data.Maybe
import Prelude ()
import Data.Traversable (sequenceA)
import Control.Applicative
import Control.Monad(join)

import CNF
import Zero

-- interface to charts and generic code on top of it.

class IsChart c where
    single :: AbelianGroupZ a => Pair a -> c a
    merging :: GrammarP nt token => Bool -> c [AST nt token] -> c [AST nt token] -> c [AST nt token]
    root :: AbelianGroupZ a => c a -> a

rootCat :: IsChart c => c [AST nt token] -> Maybe nt
rootCat = fmap cat . listToMaybe . root

unitChart :: (GrammarP nt token, IsChart chart) => Bool -> token -> chart [AST nt token] 
unitChart p tok = single (map (flip L tok) <$> tokNTP p tok)

mkTreeHelp :: (GrammarP nt token, IsChart c) => [Bool] -> [token] -> c [AST nt token]
mkTreeHelp alt s = sweeps (zipWith unitChart alts s)
 where
  sweeps []  = error "can't parse the empty string, sorry"
  sweeps [p] = p
  sweeps ps  = sweeps (pairs ps alts)

  pairs []  _       = []
  pairs [p] _      = [p]
  pairs (p:q:ps) (b:bs) = (merging b p q) : pairs ps bs

  alts = cycle alt
  

mkTree, mkTree' :: (GrammarP nt token, IsChart c) => [token] -> c [AST nt token]
mkTree = mkTreeHelp [False,True]  
mkTree' = mkTreeHelp [True,False]  

powerN :: (GrammarP nt token, IsChart chart) => Bool -> Int -> token -> chart [AST nt token]
powerN p 0 t = unitChart p t
powerN p n t = merging p (powerN False (n-1) t) (powerN True (n-1) t)

powers n t = listArray (0,n) pows
   where pows = (unitChart False t, unitChart True t) : map (\(x,y) -> (merging False x y, merging True x y)) pows 





