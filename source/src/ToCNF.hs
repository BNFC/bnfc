module ToCNF (toCNF,generate) where

import CF
import Control.Monad.State
import Control.Applicative hiding (Const)
import qualified Data.Map as M
import Data.List (nub,intercalate)
import Data.Maybe (maybeToList)
import Data.Function (on)
import Data.Char (isAlphaNum,ord)

data WithType = WithType { catTyp :: Exp, catIdent :: Cat}


instance Eq WithType where
  (==) = (==) `on` catIdent

instance Ord WithType where
  compare = compare `on` catIdent

catTyp' (Left c) = catTyp c
catTyp' (Right x) = App x []

instance Show WithType where
  show (WithType t c) = c
  
  

onRules f (CFG (exts,rules)) = CFG (exts,f rules)



toCNF cf0 = (cf2,units)
  where cf1 = onRules (delNull . toBin) . typedCats . funToExp $ cf0
        cf2 = onRules (filter (not . isUnitRule)) cf1
        units = unitSet . snd . unCFG $ cf1
    
funToExp :: CFG Fun -> CFG Exp
funToExp = fmap toExp

toExp x = App x []

typedCats = mapCatCFG (\c -> WithType (toExp c) c)

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
  let cat' = WithType (App "->" [catTyp' l, catTyp cat]) nm
  r' <- toBinRul $ Rule (f,(cat',p))
  return $ Rule (toExp "($)", (cat, [Left cat',l]))
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
                             
lk cat nullset = maybe [] id (M.lookup cat nullset)
        
nullStep :: Ord cat => [Rul' cat Exp] -> Nullable cat -> Nullable cat
nullStep rs nullset = M.unionsWith (∪) (map (uncurry M.singleton . nullRule nullset) rs)
  
nullRule :: Ord cat => Nullable cat -> Rul' cat Exp -> (cat,[Exp])
nullRule nullset (Rule (f,(c,rhs))) = (c,map (\xs -> (App "($)" (f:xs))) (cross (map nulls rhs)))
    where nulls (Right tok) = []
          nulls (Left cat) = lk cat nullset


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
delNullable :: (Show cat, Ord cat) => Nullable cat -> Rul' cat Exp -> [Rul' cat Exp]
delNullable nullset r@(Rule (f,(cat,rhs)))
  | nullable nullset r = []
  | length rhs == 1 = [r] -- here we know not element rhs is nullable, so there is nothing to do
  | otherwise = case rhs of
    [r1,r2] -> [r] ++ [Rule (app'  f x,(cat,[r2])) | x <- lk' r1]
                   ++ [Rule (flip' f x,(cat,[r1])) | x <- lk' r2]
    _ -> error $ "Panic:" ++ show r ++ "should have two elements."
  where flip' x y = App "flip" [x,y]
        app' x y = App "($)" [x,y]
        lk' (Right tok) = []
        lk' (Left cat) = lk cat nullset
        
        
delNull :: (Show cat, Ord cat) => [Rul' cat Exp] -> [Rul' cat Exp]
delNull rs = concatMap (delNullable (nullSet rs)) rs


---------------
-- UNIT

type UnitRel cat = M.Map (Either cat String) [(Exp,cat)] 

-- (c,(f,c')) ∈ unitSet   ⇒  f : c → c'

unitSetStep :: Ord cat => [Rul' cat Exp] -> UnitRel cat -> UnitRel cat
unitSetStep rs unitSet = M.unionsWith (∪) (map unitRule rs)
 where unitRule (Rule (f,(c,[r]))) = case r of 
         Right tok -> M.singleton (Right tok) [(f,c)]
         Left cat -> M.singleton (Left cat) $ (f,c) : [(comp' g f,c') | (g,c') <- lk (Left c) unitSet]
       unitRule _ = M.empty
                
unitSet rs = case fixk (unitSetStep rs) M.empty of
  Left _ -> error "Could not find fixpoint of unit set"
  Right x -> x

comp' :: Exp -> Exp -> Exp
comp' g f = App "(.)" [f,g]

isUnitRule (Rule (f,(c,[r]))) = True
isUnitRule _ = False

generate (CFG (exts,rules),units) = unlines [genCatTags rules,
                                             genCombTable rules]

genCombTable :: [Rul' WithType Exp] -> String
genCombTable rs = unlines $ map alt rs ++ ["combine _ _ = []"]


allToks = concatMap toks
 where  toks (Rule (f,(c,rhs))) = Left c : rhs
  
genCatTags :: [Rul' WithType Exp] -> String
genCatTags rs = "data CATEGORY = " ++ intercalate " | " (map catTag (allToks rs))

alt (Rule (f,(c,[r1,r2]))) = "combine " ++ catTag r1 ++ " " ++ catTag r2 ++ " = Just (" ++ catTag (Left c) ++ ", "++show f++")" -- TODO: take into account the unit to generate directly a list of results.

catTag :: Either WithType String -> String
catTag (Left c) = "CAT_" ++ concatMap escape (catIdent c)
catTag (Right t) = "TOK_" ++ concatMap escape t

genTokens :: 

escape c | isAlphaNum c = [c]
escape c = show $ ord c

                