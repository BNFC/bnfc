{-
    Copyright (C) 2012  Authors: 
    Jean-Philippe Bernardy.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{-# LANGUAGE OverloadedStrings #-}

module ToCNF (generate, genTestFile, genBenchmark) where

{-

Construction of CYK tables. The algorithm follows:

Lange, Martin; Leiß, Hans (2009), "To CNF or not to CNF? An Efficient
Yet Presentable Version of the CYK Algorithm", Informatica Didactica

-}




import CF hiding (App,Exp)
import HsOpts
import Control.Monad.RWS
import Control.Applicative hiding (Const)
import qualified Data.Map as M
import Data.List (nub,intercalate,sortBy,sort)
import Data.Maybe (maybeToList)
import Data.Function (on)
import Data.Char (isAlphaNum,ord)
import Data.String
import Data.Pair
import Text.PrettyPrint.HughesPJ hiding (first,(<>))

(f *** g) (a,b) = (f a, g b)
second g = id *** g

onRules f (CFG (exts,rules)) = CFG (exts,f rules)

toCNF cf0 = (cf1,cf2,units,descriptions)
  where cf01@(CFG (exts01,_)) = funToExp . onRules delInternal $ cf0
        (rules',descriptions) = toBin (rulesOfCF cf01) 
        cf1 = CFG (exts01,rules')
        cf2 = onRules delNull cf1
        units = unitSet . snd . unCFG $ cf2
    
funToExp :: CFG Fun -> CFG Exp
funToExp = fmap toExp

delInternal = filter (not . isInternalRhs . rhsRule)
  where isInternalRhs (Left c:_) = c == internalCat
        isInternalRhs _ = False

--------------------------------------------------------------
-- BIN: make sure no rule has more than 2 symbols on the rhs

allocateCatName = do
  n <- get
  put (1+n)
  return $ show n

toBin :: [Rul Exp] -> ([Rul Exp], CatDescriptions)
toBin cf = (a,w)
  where (a,_,w) = runRWS (concat <$> forM cf toBinRul) () 0
        
catName = either id id

type CatDescriptions = M.Map Cat Doc

-- | Convert a rule into a number of equivalent rules with at most 2
-- symbols on the rhs.
-- Also writes an explanation of what new categories are.
toBinRul :: Rul Exp -> RWS () CatDescriptions Int [Rul Exp]
toBinRul (Rule f cat rhs) | length rhs > 2 = do
  cat' <- allocateCatName
  r' <- toBinRul $ Rule f cat' p
  tell $ M.singleton cat' (int (length p) <> "-prefix of " <> prettyExp f <> " " <> parens (prettyRHS p))
  return $ Rule (Con "($)") cat [Left cat',l]
         : r'
  where l = last rhs
        p = init rhs
        fun' = case l of
          Left _ -> Con "($)" -- in this case we have to apply the final argument to the partial result
          Right _ -> Con "const" -- in this case the 2nd argument must be ignored (it is not present in the result).
toBinRul r = return [r]


prettyRHS = hcat . punctuate " " . map (either text (quotes . text))

-------------------------------------------------------
-- DEL : make sure no rule has 0 symbol on the rhs

type Nullable cat = M.Map cat [Exp]

cross :: [[a]] -> [[a]]
cross [] = [[]]
cross (x:xs) = [y:ys | y <- x,  ys <- cross xs]

x ∪ y = sort $ nub (x ++ y)
                             
lk cat nullset = maybe [] id (M.lookup cat nullset)
        
nullStep :: [Rul Exp] -> Nullable Cat -> Nullable Cat
nullStep rs nullset = M.unionsWith (∪) (map (uncurry M.singleton . nullRule nullset) rs)
  
nullRule :: Nullable Cat -> Rul Exp -> (Cat,[Exp])
nullRule nullset (Rule f c rhs) = (c,map (\xs -> (appMany f xs)) (cross (map nulls rhs)))
    where nulls (Right tok) = []
          nulls (Left cat) = lk cat nullset


nullable :: Nullable Cat -> Rul Exp -> Bool
nullable s = not . null . snd . nullRule s

fixk :: Eq a => (a -> a) -> a -> Either a a
fixk = fixn 100

fixn :: Eq a => Int -> (a -> a) -> a -> Either a a
fixn 0 f x = Left x
fixn n f x = if x' == x then Right x else fixn (n-1) f x'
  where x' = f x
        
nullSet :: [Rul Exp] -> Nullable Cat   
nullSet rs = case fixk (nullStep rs) M.empty of
  Left x -> error "Could not find fixpoint of nullable set"
  Right x -> x


-- | Replace nullable occurences by nothing, and adapt the function consequently.
delNullable :: Nullable Cat -> Rul Exp -> [Rul Exp]
delNullable nullset r@(Rule f cat rhs) = case rhs of
  [] -> []
  [_] -> [r] 
  [r1,r2] -> [r] ++ [Rule (app'  f x) cat [r2] | x <- lk' r1]
                 ++ [Rule (app2 (isCat r1) f x) cat [r1] | x <- lk' r2]
  _ -> error $ "Panic:" ++ show r ++ "should have at most two elements."
  where lk' (Right tok) = []
        lk' (Left cat) = lk cat nullset
        
        
delNull :: [Rul Exp] -> [Rul Exp]
delNull rs = concatMap (delNullable (nullSet rs)) rs


---------------
-- UNIT

type UnitRel cat = M.Map (Either cat String) [(Exp,cat)] 

-- (c,(f,c')) ∈ unitSet   ⇒  f : c → c'

unitSetStep :: [Rul Exp] -> UnitRel Cat -> UnitRel Cat
unitSetStep rs unitSet = M.unionsWith (∪) (map unitRule rs)
 where unitRule (Rule f c [r]) = M.singleton r $ (f,c) : [(g `appl` f,c') | (g,c') <- lk (Left c) unitSet]
         where appl = case r of
                 Left _ -> after
                 Right _ -> app'
       unitRule _ = M.empty
                
unitSet :: [Rul Exp] -> UnitRel Cat                  
unitSet rs = case fixk (unitSetStep rs) M.empty of
  Left x -> error $ "Could not find fixpoint of unit set. Last iteration:\n" ++ render (prettyUnitSet x)
  Right x -> x

isUnitRule (Rule f c [r]) = True
isUnitRule _ = False


------------------------
-- Left/Right occurences

isOnLeft, isOnRight :: RHSEl -> Rul f -> Bool
isOnLeft c (Rule f _ [c',_]) = c == c'
isOnLeft _ _ = False

isOnRight c (Rule f _ [_,c']) = c == c'
isOnRight _ _ = False

isEntryPoint cf el = either (`elem` allEntryPoints cf) (const False) el

occurs :: (RHSEl -> Rul f -> Bool) -> RHSEl -> CFG f -> Bool
occurs where_ el cf = any (where_ el) (rulesOfCF cf)

splitLROn :: (a -> RHSEl) -> CFG f -> [a] -> Pair [a]
splitLROn f cf xs = filt <*> pure xs
  where filt = filter (\c -> occurs isOnLeft  (f c) cf || isEntryPoint cf (f c)) :/: 
               filter (\c -> occurs isOnRight (f c) cf)
        
isSpecial (Left ('@':'@':_)) = True
isSpecial _ = False
        
optim :: (a -> RHSEl) -> Pair [a] -> Pair [(a,Doc -> Doc)]
optim f (x:/:y) = map modif x :/: map modif' y
  where modif  a | isSpecial (f a) = (a,\x -> "(if not p then (" <> x <> ":) else id)")
                 | otherwise = (a,rob) 
        modif' a | isSpecial (f a) = (a,\x -> "(if     p then (" <> x <> ":) else id)")
                 | otherwise = (a,rob)
        rob x = "("<> x <> ":)"
                                      

splitOptim f cf xs = optim f $ splitLROn f cf $ xs


---------------------------
-- Error reporting

-- lrSet (Rule f c [r1,r2]) = c 


-------------------------
-- Code generation

incomment x = "{-" <> x <> "-}"

generate opts cf0 = render $ vcat [header opts
                                  ,genShowFunction cf0
                                  ,genCatTags cf1
                                  ,genDesc cf1 descriptions
                                  ,genCombTable units (onRules (filter (not . isUnitRule)) cf)
                                  ,genTokTable units cf
                                  ,incomment $ vcat 
                                   ["Normalised grammar:"
                                   ,text $ show cf
                                   ,"Unit relation:"
                                   ,prettyUnitSet units
                                   ]
                                  ]
  where (cf1,cf,units,descriptions) = toCNF cf0

prettyUnitSet units = vcat [prettyExp f <> " : " <> catTag cat <> " --> " <> text cat' | (cat,x) <- M.assocs units, (f,cat') <- x] 

header opts
       = vcat ["{-# LANGUAGE MagicHash, FlexibleInstances #-}"
              ,"module " <> text (cnfTablesFileM opts) <> " where"
              ,"import GHC.Prim"
              ,"import GHC.Exts"
              ,"import Control.Applicative hiding (Const)"
              ,"import Algebra.RingUtils"
              ,"import Parsing.Chart ()"
              ,"import " <> text (absFileM  opts)
              ,"import " <> text (alexFileM opts)
              ,"import " <> text ( printerFileM  opts)
              ,"readInteger :: String -> Integer"
              ,"readInteger = read"
              ,"readDouble :: String -> Double"
              ,"readDouble = read"
              ,"instance RingP [(CATEGORY,Any)] where"
              ,"  mul p a b = trav [map (app tx ty) l :/: map (app tx ty) r | (x,tx) <- a, (y,ty) <- b, let l:/:r = combine p x y]"
              ,"    where trav :: [Pair [a]] -> Pair [a]"
              ,"          trav [] = pure []"
              ,"          trav (x:xs) = (++) <$> x <*> trav xs"
              ,"          app tx ty (z,f)  = (z, f tx ty)"
              ]

punctuate' p = cat . punctuate p

genShowFunction cf = hang "showAst (cat,ast) = case cat of " 6 
       (vcat [catTag (Left cat) <> " -> printTree ((unsafeCoerce# ast)::" <> text cat <> ")"
             | cat <- filter isDataCat $ allCats cf] $$ 
        "_ -> \"Unprintable category\"")


genCatTags :: CFG Exp -> Doc
genCatTags cf = "data CATEGORY = " <> punctuate' "|" (map catTag (allSyms cf)) $$
                "  deriving (Eq,Ord,Show)"

genDesc :: CFG Exp -> CatDescriptions -> Doc
genDesc cf descs = vcat ["describe " <> catTag s <> " = " <> doubleQuotes (descOf s) | s <- allSyms cf]
  where descOf (Right x) = "token " <> text x
        descOf (Left x) = maybe (text x) id $ M.lookup x descs
  
  

genCombTable :: UnitRel Cat -> CFG Exp -> Doc
genCombTable units cf = 
     "combine :: Bool -> CATEGORY -> CATEGORY -> Pair [(CATEGORY, Any -> Any -> Any)]"
  $$ genCombine units cf
  $$ "combine _ _ _ = pure []"

allSyms :: CFG Exp -> [Either String String]
allSyms cf = map Left (allCats cf  ++ literals cf) ++ map (Right . fst) (cfTokens cf)
        

ppPair (x,y) = parens $ x <> comma <> " " <> y

unsafeCoerce' = app' (Con "unsafeCoerce#")

type RHSEl = Either Cat String

isCat (Right _) = False
isCat (Left _) = True

group' :: Eq a => [(a,[b])] -> [(a,[b])]
group' [] = []
group' ((a,bs):xs) = (a,bs ++ concatMap snd ys) : group' zs
  where (ys,zs) = span (\x -> fst x == a) xs

prettyPair (x :/: y) = sep [x,":/:",y]
prettyListFun xs = parens $ sep (map (<> "$") xs) <> "[]"


genCombine :: UnitRel Cat -> CFG Exp -> Doc
genCombine units cf = vcat $ map genEntry $ group' $ sortBy (compare `on` fst) $ map (alt units) (rulesOfCF cf)
  where genEntry :: ((RHSEl,RHSEl),[(Cat,Exp)]) -> Doc
        genEntry ((r1,r2),cs) = "combine p " <> catTag r1 <> " " <> catTag r2 <> " = " <> prettyPair (genList <$> splitOptim (Left . fst) cf cs)
        mkLam body = "\\x y -> " <> body
        genList xs = prettyListFun [p (ppPair (catTag . Left $ x, mkLam . prettyExp . unsafeCoerce' $ y)) | ((x,y),p) <- xs]

alt :: UnitRel Cat -> Rul Exp -> ((RHSEl,RHSEl),[(Cat,Exp)])
alt units (Rule f c [r1,r2]) = ((r1,r2),initial:others)
  where initial = (c, f `appMany` args)
        others = [(c', f' `app'` (f `appMany` args)) | (f',c') <- lk (Left c) units]
        args = map (unsafeCoerce' . Con) $ ["x"|isCat r1]++["y"|isCat r2]
    
catTag :: Either String String -> Doc
catTag (Left c) = "CAT_" <> text (concatMap escape c)
catTag (Right t) = "TOK_" <> text (concatMap escape t)

escape c | isAlphaNum c || c == '_' = [c]
escape '[' = ""
escape ']' = "_List"
escape '{' = "OPEN_"
escape '}' = "CLOS_"
escape '@' = "BIN_"
escape c = show $ ord c

genTokTable :: UnitRel Cat -> CFG Exp -> Doc
genTokTable units cf = "tokenToCats :: Token -> Pair [(CATEGORY,Any)]" $$
                       vcat (map (genSpecEntry cf units) (tokInfo cf)) $$
                       vcat (map (genTokEntry cf units) (cfTokens cf)) $$
                       "tokenToCats t = error (\"unknown token: \" ++ show t)"

tokInfo cf = ("Char","TC",Con "head"):
             ("String","TL",Id):("Integer","TI",Con "readInteger"):
             ("Double","TD",Con "readDouble"):
             [("Ident","TV",Con "Ident")|hasIdent cf] ++
             [(t,"T_" <> text t,(Con t)) | t <- tokenNames cf]

genTokCommon cf xs = prettyPair (gen <$> splitOptim fst cf xs)
  where gen ys = prettyListFun [p (ppPair (catTag x,y)) | ((x,y),p) <- ys]

genSpecEntry cf units (tokName,constrName,fun) = "tokenToCats (PT (Pn _ l c) (" <> constrName <> " x)) = " <> genTokCommon cf xs
  where xs = map (second (prettyExp . (\f -> unsafeCoerce' (f `app'` tokArgs)))) $ 
             (Left tokName, fun) : [(Left c,f `after` fun) | (f,c) <- lk (Left tokName) units]
        tokArgs | isPositionCat cf tokName = Con "((l,c),x)"
                | otherwise = Con "x"

genTokEntry cf units (tok,x) = 
  " -- " <> text tok $$
  "tokenToCats (PT posn (TS _ " <> int x <> ")) = " <> genTokCommon cf xs
  where xs = (Right tok, tokVal) : 
             [(Left c,prettyExp (unsafeCoerce' f)) | (f,c) <- lk (Right tok) units]
        tokVal = "error" <> (text $ show $ "cannot access value of token: " ++ tok)
  
------------------------           
-- Test file generation

genTestFile opts cf = render $ vcat
    ["module Main where"
    ,"import " <> text ( alexFileM     opts)
    ,"import " <> text ( cnfTablesFileM opts)
    ,"import Parsing.TestProgram"
    ,"main = mainTest showAst tokenToCats tokens tokenLineCol describe"]


genBenchmark opts = render $ vcat
   ["import System.Environment ( getArgs )"
   ,"import "<> text ( alexFileM opts) <> " as Lexer"
   ,"import "<> text ( cnfTablesFileM opts) <> " as Parser"
   ,"import GHC.Exts"
   ,"import Parsing.Chart"
   ,"import Criterion.Main"
   ,"import Algebra.RingUtils"
   ,"import Control.Applicative"
   ,"type T = [(CATEGORY,Any)]"
   ,"pLGrammar :: [Pair T] -> MT2 T"
   ,"pLGrammar = mkTree"
   ,"main = do"
   ,"  f:_ <- getArgs"
   ,"  s <- readFile f"
   ,"  let ts = map tokenToCats $ Lexer.tokens s"
   ,"      (ts1,x:ts2) = splitAt (length ts `div` 2) ts"
   ,"      cs = [mkTree ts1,mkTree' ts2]"
   ,"      work [c1,c2] = show $ map fst $ root $ mergein False c1 x c2"
   ,"  defaultMain [bench f $ nf work cs] -- note the hack!!!"
   ]


           
---------------------------------           
-- Management of expressions. 

-- Most of this is not strictly useful; its main purpose is to
-- generate "nice-looking" semantic actions

data Exp = Id -- identity function
          | Con String -- constructor or variable
          | App Exp Exp
          | Exp `After` Exp
          | App2 Exp Exp
            deriving (Eq,Ord)

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