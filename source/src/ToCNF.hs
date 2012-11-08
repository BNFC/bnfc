{-# LANGUAGE OverloadedStrings #-}

module ToCNF (toCNF,generate) where

import CF hiding (App,Exp)
import Control.Monad.State
import Control.Applicative hiding (Const)
import qualified Data.Map as M
import Data.List (nub,intercalate,sortBy)
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

toCNF cf0 = (cf1,units)
  where cf1 = onRules (delNull . toBin) . typedCats . funToExp . onRules delInternal $ cf0
        units = unitSet . snd . unCFG $ cf1
    
funToExp :: CFG Fun -> CFG Exp
funToExp = second toExp

typedCats = first (\c -> WithType (toExp c) c)

delInternal = filter (not . isInternalRhs . rhsRule)
  where isInternalRhs (Left c:_) = c == internalCat
        isInternalRhs _ = False

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
  return $ Rule (Con "($)", (cat, [Left cat',l])) 
         : r'
  where l = last rhs
        p = init rhs
        fun' = case l of
          Left _ -> Con "($)" -- in this case we have to apply the final argument to the partial result
          Right _ -> Con "const" -- in this case the 2nd argument must be ignored (it is not present in the result).
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
                   ++ [Rule (app2 (isCat r1) f x,(cat,[r1])) | x <- lk' r2]
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

generate opts
          (cf@(CFG (exts,rules)),units) = render $ vcat [header opts,
                                                        genCatTags cf,
                                                        genCombTable units (filter (not . isUnitRule) rules),
                                                        genTokTable units cf]

header opts
       = vcat ["{-# LANGUAGE MagicHash #-}"
              ,"module ParseTables where"
              ,"import GHC.Prim"
              ,"import GHC.Exts"
              ,"import Absalfa" 
              ,"import Lexalfa(Tok(..))" 
              ,"readInteger :: String -> Integer"
              ,"readInteger = read"
              ]

punctuate' p = cat . punctuate p

genCatTags :: CFG' WithType Exp -> Doc
genCatTags cf = "data CATEGORY = " <> punctuate' "|" (map catTag (allSyms cf))


genCombTable :: UnitRel WithType -> [Rul' WithType Exp] -> Doc
genCombTable units rs = 
     "combine :: CATEGORY -> CATEGORY -> [(CATEGORY, Any -> Any -> Any)]"
  $$ genCombine units rs
  $$ "combine _ _ = []"

allSyms :: CFG' WithType Exp -> [Either String String]
allSyms cf = map Left (map catIdent (allCats cf)  ++ literals cf) ++ map (Right . fst) (cfTokens cf)
        

ppPair (x,y) = parens $ x <> comma <> " " <> y
ppList xs = brackets $ punctuate' comma xs

unsafeCoerce' = app' (Con "unsafeCoerce#")

type RHSEl = Either WithType String

group' :: Eq a =>[(a,[b])] -> [(a,[b])]
group' [] = []
group' ((a,bs):xs) = (a,bs ++ concatMap snd ys) : group' zs
  where (ys,zs) = span (\x -> fst x == a) xs

genCombine :: UnitRel WithType -> [Rul' WithType Exp] -> Doc
genCombine units rs = vcat $ map genEntry $ group' $ sortBy (compare `on` fst) $ map (alt units) rs
  where genEntry ((r1,r2),fs) = "combine " <> catTag (first catIdent r1) <> " " <> catTag (first catIdent r2) <> " = " <> ppList (map (ppPair . bimap (catTag . Left . catIdent) (mkLam . prettyExp . unsafeCoerce')) fs)
        mkLam body = "\\x y -> " <> body

alt :: UnitRel WithType -> Rul' WithType Exp -> ((RHSEl,RHSEl),[(WithType,Exp)])
alt units (Rule (f,(c,[r1,r2]))) = ((r1,r2),initial:others)
  where initial = (c, f `appMany` args)
        others = [(c', f' `app'` (f `appMany` args)) | (f',c') <- lk (Left c) units]
        args = map (unsafeCoerce' . Con) $ ["x"|isCat r1]++["y"|isCat r2]
    
isCat (Right _) = False
isCat (Left _) = True
        

catTag :: Either String String -> Doc
catTag (Left c) = "CAT_" <> text (concatMap escape c)
catTag (Right t) = "TOK_" <> text (concatMap escape t)

genTokTable :: UnitRel WithType -> CFG' WithType Exp -> Doc
genTokTable units cf = "tokens :: (Int,Int) -> Tok -> [(CATEGORY,Any)]" $$
                       vcat (map (genSpecEntry cf units) (tokInfo cf)) $$
                       vcat (map (genTokEntry units) (cfTokens cf))

forgetTypes = first catIdent

tokInfo cf = ("String","TL",Id):("Integer","TI",Con "readInteger") : [("Ident","TV",Con "Ident")|hasIdent (forgetTypes cf)] ++
        [(t,"T_" <> text t,(Con t)) | t <- tokenNames cf]

genSpecEntry cf units (tokName,constrName,fun) = "tokens posn (" <> constrName <> " x) = " <> ppList (map ppPair xs)
  where xs = map (second (prettyExp . (\f -> unsafeCoerce' (f `app'` tokArgs)))) $ 
             (catTag (Left tokName), fun) : [(catTag (Left $ catIdent c),f `after` fun) |
              (f,c) <- lk (Left $ WithType (Con tokName) tokName) units]
        tokArgs | isPositionCat cf tokName = Con "(posn,x)"
                | otherwise = Con "x"

genTokEntry units (tok,x) = "tokens posn (TS _ " <> int x <> ") = " <> ppList (map ppPair xs)
  where xs = (catTag (Right tok), tokVal):
          [(catTag (Left $ catIdent c),prettyExp (unsafeCoerce' f)) 
          | (f,c) <- lk (Right tok) units]
        tokVal = "error" <> (text $ show $ "cannot access value of token: " ++ tok)
  
  
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

-- | Apply in 2nd position if the flag is true, otherwise apply normally.
app2 True f x =  App2 f x
app2 False f x = app' f x

infixl `app'`
app' :: Exp -> Exp -> Exp
app' (f `After` g) x = app' f (app' g x)
app' Id x = x
app' (App2 f y) x = (f `app'` x) `app'` y
app' (Con "($)") f = f
-- app' (Con "const") f = f
app' f x = App f x

toExp f | isCoercion f = Id
        | otherwise = Con f                

after :: Exp -> Exp -> Exp
after Id f = f
after f Id = f
after f g = f `After` g


appMany f args = foldl app' f args