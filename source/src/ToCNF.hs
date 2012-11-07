{-# LANGUAGE OverloadedStrings #-}

module ToCNF (toCNF,generate) where

import CF hiding (App,Exp)
import Control.Monad.State
import Control.Applicative hiding (Const)
import qualified Data.Map as M
import Data.List (nub,intercalate)
import Data.Maybe (maybeToList)
import Data.Function (on)
import Data.Char (isAlphaNum,ord)
import Data.String
import Text.PrettyPrint.HughesPJ hiding (first)
import Data.Bifunctor

data WithType = WithType { catTyp :: Exp, catIdent :: Cat}

instance Eq WithType where
  (==) = (==) `on` catIdent

instance Ord WithType where
  compare = compare `on` catIdent

catTyp' (Left c) = catTyp c
catTyp' (Right x) = Con x

instance Show WithType where
  show (WithType t c) = c
  
onRules f (CFG (exts,rules)) = CFG (exts,f rules)

toCNF cf0 = (cf2,units)
  where cf1 = onRules (delNull . toBin) . typedCats . funToExp $ cf0
        cf2 = onRules (filter (not . isUnitRule)) cf1
        units = unitSet . snd . unCFG $ cf1
    
funToExp :: CFG Fun -> CFG Exp
funToExp = second toExp

typedCats = first (\c -> WithType (toExp c) c)

--------------------------------------------------------------
-- BIN: make sure no rule has more than 2 symbols on the rhs

allocateCatName = do
  n <- get
  put (1+n)
  return $ show n

toBin :: [Rul' WithType Exp] -> [Rul' WithType Exp]
toBin cf = fst $ runState (concat <$> forM cf toBinRul) 0

catName = either id id

-- | Convert a rule into a number of equivalent rules with at most 2
-- symbols on the rhs
toBinRul :: Rul' WithType  Exp -> State Int [Rul' WithType Exp]
toBinRul (Rule (f,(cat,rhs))) | length rhs > 2 = do
  nm <- allocateCatName
  let cat' = WithType (appMany (Con "->") [catTyp' l, catTyp cat]) nm
  r' <- toBinRul $ Rule (f,(cat',p))
  return $ Rule (Con "($)", (cat, [Left cat',l])) -- FIXME: do the application only if the rhs in not a single token.
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
nullRule nullset (Rule (f,(c,rhs))) = (c,map (\xs -> (appMany f xs)) (cross (map nulls rhs)))
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
                   ++ [Rule (App2  f x,(cat,[r1])) | x <- lk' r2]
    _ -> error $ "Panic:" ++ show r ++ "should have two elements."
  where lk' (Right tok) = []
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
         Left cat -> M.singleton (Left cat) $ (f,c) : [(g `after` f,c') | (g,c') <- lk (Left c) unitSet]
       unitRule _ = M.empty
                
unitSet rs = case fixk (unitSetStep rs) M.empty of
  Left _ -> error "Could not find fixpoint of unit set"
  Right x -> x

isUnitRule (Rule (f,(c,[r]))) = True
isUnitRule _ = False

-------------------------
-- Code generation

generate (cf@(CFG (exts,rules)),units) = render $ vcat [header,
                                                        genCatTags cf,
                                                        genCombTable units rules,
                                                        genTokTable units cf]

header = vcat ["module ParseTables where"
              ,"import GHC.Prim"
              ,"readInteger :: String -> Integer"
              ,"readInteger = read"
              ]

genCombTable :: UnitRel WithType -> [Rul' WithType Exp] -> Doc
genCombTable units rs = 
     "combine :: CATEGORY -> CATEGORY -> [(CATEGORY, Any -> Any -> Any)]"
  $$ vcat (map (alt units) rs) 
  $$  "combine _ _ = []"

punctuate' p = cat . punctuate p

allSyms :: CFG' WithType Exp -> [Either String String]
allSyms cf = map Left (map catIdent (allCats cf)  ++ literals cf) ++ map (Right . fst) (cfTokens cf)
        
genCatTags :: CFG' WithType Exp -> Doc
genCatTags cf = "data CATEGORY = " <> punctuate' "|" (map catTag (allSyms cf))


ppPair (x,y) = parens $ x <> comma <> " " <> y
ppList xs = brackets $ punctuate' comma xs

alt :: UnitRel WithType -> Rul' WithType Exp -> Doc
alt units (Rule (f,(c,[r1,r2]))) = "combine " <> catTag (first catIdent r1) <> " " <> catTag (first catIdent r2) <> " = " <> ppList (map (ppPair . second (mkLam . prettyExp)) (initial:others))
  where initial = (catTag (Left $ catIdent c), f `appMany` args)
        others = [(catTag (Left $ catIdent c'), f' `app'` (f `appMany` args)) | (f',c') <- lk (Left c) units]
        args = map Con $ ["x"|isCat r1]++["y"|isCat r2]
        mkLam body = "\\x y -> " <> body
                             
isCat (Left _) = True
isCat (Right _) = False
        

catTag :: Either String String -> Doc
catTag (Left c) = "CAT_" <> text (concatMap escape c)
catTag (Right t) = "TOK_" <> text (concatMap escape t)

genTokTable units cf = vcat $
                       ["tokens :: Tok -> [(CATEGORY,String -> Any)]"
                       ,"tokens (TL x) = [(CAT_String,id)]"
                       ,"tokens (TI x) = [(CAT_Integer,readInteger)]"
                       ,"tokens (TV x) = [(CAT_Ident,Ident)]"
                        ] ++
                       map (genTokEntry units) (cfTokens cf)

genTokEntry units (tok,x) = "tokens (TS _ " <> int x <> ") = " <> ppList (map ppPair xs)
  where xs = (catTag (Right tok), tokVal):
          [(catTag (Left $ catIdent c),prettyExp f `app` Con "err?") 
          | (f,c) <- lk (Right tok) units]
        tokVal = "error" <> doubleQuotes (text $ "cannot access value of token: " ++ tok)
  
  
escape c | isAlphaNum c = [c]
escape '[' = ""
escape ']' = "_List"
escape c = show $ ord c

                
           
--------           

data Exp = Id -- identity function
          | Con String -- constructor or variable
          | App Exp Exp
          | Exp `After` Exp
          | App2 Exp Exp
            deriving (Eq)

prettyExp Id = "id"
prettyExp (Con x) = text x
prettyExp (App f x) = prettyExp f <+> (parens $ prettyExp x)
prettyExp (App2 f x) = "flip" <+> parens (prettyExp f) <+> parens (prettyExp x)
prettyExp (f `After` g) = parens (prettyExp f) <> "." <> parens (prettyExp g)

instance Show Exp where show = render . prettyExp

app' :: Exp -> Exp -> Exp
app' (f `After` g) x = app' f (app' g x)
app' Id x = x
app' (App2 f y) x = (f `app'` x) `app'` y
app' (Con "($)") f = f
app' f x = App f x

toExp f | isCoercion f = Id
        | otherwise = Con f                

after :: Exp -> Exp -> Exp
after Id f = f
after f Id = f
after f g = f `After` g


appMany f args = foldl app' f args