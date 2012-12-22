module BNFC.Runtime.CYKSparse where

import Data.Array.IArray
import Data.List
import Data.Char
import Data.Maybe
import GHC.Exts


-- Sparse triangle matrices.
data Chart a = 
    Single a | 
    Merge {
      mergeSize :: !Int,
      here :: Array Int [(Int,a)], -- parses starting in left, ending in right.
      left :: Chart a,
      right :: Chart a
    } -- invariant : mergeSize = size left + size right
    deriving Show 

-- | Size of a chart
size :: Chart a -> Int
size (Single _) = 1
size (Merge s _ _ _) = s


-- | Everything that starts at a given position
startingAt = startingAt' True

-- | Things that start at a given position. Flag=False means not to take anything from the left subtree.
startingAt' :: Bool -> Int -> Chart a -> [(Int,a)]
startingAt' _ 0 (Single nt) = [(1,nt)]
startingAt' _ e (Single nt) = []
startingAt' lookLeft s (Merge n h l r) 
    | s >= n = []
    | s >= size l = map (shift (size l)) (startingAt (s-size l) r)
    | lookLeft = (h ! s) ++  (startingAt s l)
    | otherwise = h ! s
 where shift i (x,y) = (x+i,y)

-- | Merge two charts.
merge (*) l r = result
    where
      result = Merge n h l r 
      n = size l + size r
      h = listArray (0,size l-1) $
            map (\start -> concatMap (init . chainsFrom) (startingAt start l))
                -- the last element is the one taken from the left
                -- part; so drop it.
            [0..size l - 1]
      -- chainsFrom :: (Int,Set (AST nt tok)) -> [(Int,Set (AST nt tok))]
      chainsFrom (mid,nts1) = [x |
                               (end,nts2) <- startingAt' False mid result,
                               -- we don't want things included in the
                               -- left part: merging those would
                               -- return things included in the left
                               -- part again.
                               let nts = nts1 * nts2,
                               not (null nts),
                               x <- chainsFrom (end,nts)
                              ] ++ [(mid,nts1)]
      -- it's tempting to sort the above list by large ends first, but
      -- that would destroy laziness. Things will be "more or less" sorted anyway.
              

type AST cat = (cat,Any)



mkTree :: (posn -> tok -> [(cat,Any)]) -> 
          (cat -> cat -> [(cat, Any -> Any -> Any)]) -> [(posn,tok)] -> Chart [AST cat]
mkTree tokens combine ts = sweeps (map unitChart ts)
 where
  sweeps []  = error "can't parse the empty string, sorry"
  sweeps [p] = p
  sweeps ps  = sweeps (pairs ps)

  pairs []        = []
  pairs [p]       = [p]
  pairs (p:q:ps)  = (merge mul p q) : pairs ps 
  
  mul p q = [(c',f x1 x2) | (c1,x1) <- p, (c2,x2) <- q, (c',f) <- combine c1 c2]

  -- unitChart :: (Posn,tok) -> Chart [AST cat]
  unitChart (posn,tok) = Single (tokens posn tok)
