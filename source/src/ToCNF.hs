module ToCNF where

import CF
import Control.Monad.State
import Control.Applicative hiding (Const)
import qualified Data.Map as M
import Data.List (nub)
import Data.Maybe (maybeToList)
import Data.Function (on)

data WithType = WithType { catTyp :: Exp, catIdent :: Cat}


instance Eq WithType where
  (==) = (==) `on` catIdent

instance Ord WithType where
  compare = compare `on` catIdent

catTyp' (Left c) = catTyp c
catTyp' (Right x) = Const x

instance Show WithType where
  show (WithType t c) = c
  
  

onRules f (CFG (exts,rules)) = CFG (exts,f rules)

toCNF cf0 = cf1
  where cf1 = onRules (toBin) . typedCats . funToExp $ cf0
    
funToExp :: CFG Fun -> CFG Exp
funToExp = fmap Const

typedCats = mapCatCFG (\c -> WithType (Const c) c)

--------------------------------------------------------------
-- BIN: make sure no rule has more than 2 symbols on the rhs

allocateCatName = do
  n <- get
  put (1+n)
  return $ "C__" ++ show n

toBin :: [Rul' WithType Exp] -> [Rul' WithType Exp]
toBin cf = fst $ runState (concat <$> forM cf toBinRul) 0

catName = either id id

-- | Convert a rule into a number of equivalent rules with at most 2
-- symbols on the rhs
toBinRul :: Rul' WithType  Exp -> State Int [Rul' WithType Exp]
toBinRul (Rule (f,(cat,rhs))) | length rhs > 2 = do
  nm <- allocateCatName
  let cat' = WithType (App (Const "->") [catTyp' l, catTyp cat]) nm
  r' <- toBinRul $ Rule (f,(cat',p))
  return $ Rule (Const "($)", (cat, [Left cat',l]))
         : r'
  where l = last rhs
        p = init rhs
toBinRul r = return [r]


-------------------------------------------------------
-- DEL : make sure no rule has 0 symbol on the rhs

type Nullable cat = M.Map cat [Exp]

cross :: [[a]] -> [[a]]
cross [] = [[]]
cross (x:xs) = [y:ys | y <- x,  ys <- cross xs]

x ∪ y = nub (x ++ y)
                             
lkNull cat nullset = maybe [] id (M.lookup cat nullset)
        
nullStep :: Ord cat => [Rul' cat Exp] -> Nullable cat -> Nullable cat
nullStep rs nullset = M.unionsWith (∪) (map (uncurry M.singleton . nullRule nullset) rs)
  
                                    
nullRule nullset (Rule (f,(c,rhs))) = (c,map (App f) (cross (map nulls rhs)))
    where nulls (Right tok) = []
          nulls (Left cat) = lkNull cat nullset


nullable :: Ord cat => Nullable cat -> Rul' cat Exp -> Bool
nullable s = not . null . snd . nullRule s

fixk :: Eq a => (a -> a) -> a -> Either String a
fixk = fixn 100

fixn :: Eq a => Int -> (a -> a) -> a -> Either String a
fixn 0 f x = Left "Could not find fixpoint"
fixn n f x = if x' == x then Right x else fixn (n-1) f x'
  where x' = f x
        
nullSet :: Ord cat => [Rul' cat Exp] -> Nullable cat   
nullSet rs = case fixk (nullStep rs) M.empty of
  Left x -> error "Could not find fixpoint of nullable set"
  Right x -> x

-- | Replace nullable occurences by nothing, and adapt the function consequently.
delNullable :: Ord cat => Nullable cat -> Rul' cat Exp -> [Rul' cat Exp]
delNullable nullset ~r@(Rule (f,(cat,rhs@[r1,r2])))
  | nullable nullset r = []
  | length rhs == 1 = [r] -- here we know not element rhs is nullable, so there is nothing to do
  | otherwise = [r] ++ [Rule (App f [x],(cat,[r2])) | x <- lkNull' r1]
                    ++ [Rule (App (flip' f) [x],(cat,[r1])) | x <- lkNull' r2]
  where flip' x = App (Const "flip") [x]
        lkNull' (Right tok) = []
        lkNull' (Left cat) = lkNull cat nullset
        
        
delNull :: Ord cat => [Rul' cat Exp] -> [Rul' cat Exp]
delNull rs = concatMap (delNullable (nullSet rs)) rs


