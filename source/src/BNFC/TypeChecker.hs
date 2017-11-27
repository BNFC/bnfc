
module BNFC.TypeChecker where

import Control.Monad

import Data.Char
import Data.Function (on)
import Data.List

import BNFC.CF
import ErrM

data Base = BaseT String
          | ListT Base
    deriving (Eq)

data Type = FunT [Base] Base
    deriving (Eq)

instance Show Base where
    show (BaseT x) = x
    show (ListT t) = "[" ++ show t ++ "]"

instance Show Type where
    show (FunT ts t) = unwords $ map show ts ++ ["->", show t]

data Context = Ctx
  { ctxLabels :: [(String, Type)]
  , ctxTokens :: [String]
  }

catchErr :: Err a -> (String -> Err a) -> Err a
catchErr (Bad s) f = f s
catchErr (Ok x) _  = Ok x

buildContext :: CF -> Context
buildContext cf@CFG{..} = Ctx
  { ctxLabels =
      [ (f, mkType cat args)
        | Rule f cat args <- cfgRules
        , not (isCoercion f)
        , not (isNilCons f)
      ]
  , ctxTokens =
      ("Ident" : tokenNames cf)
  }
  where
    mkType cat args = FunT [ mkBase t | Left t <- args, t /= InternalCat ]
                           (mkBase cat)
    mkBase t
        | isList t  = ListT $ mkBase $ normCatOfList t
        | otherwise = BaseT $ show $ normCat t

isToken :: String -> Context -> Bool
isToken x ctx = elem x $ ctxTokens ctx

extendContext :: Context -> [(String,Type)] -> Context
extendContext ctx xs = ctx { ctxLabels = xs ++ ctxLabels ctx }

lookupCtx :: String -> Context -> Err Type
lookupCtx x ctx
    | isToken x ctx = return $ FunT [BaseT "String"] (BaseT x)
    | otherwise     =
    case lookup x $ ctxLabels ctx of
        Nothing -> fail $ "Undefined symbol '" ++ x ++ "'."
        Just t  -> return t

checkDefinitions :: CF -> Err ()
checkDefinitions cf =
    do  checkContext ctx
        sequence_ [checkDefinition ctx f xs e | FunDef f xs e <- cfgPragmas cf]
    where
        ctx = buildContext cf

checkContext :: Context -> Err ()
checkContext ctx =
    mapM_ checkEntry $ groupSnd $ ctxLabels ctx
    where
        -- This is a very handy function which transforms a lookup table
        -- with duplicate keys to a list valued lookup table with no duplicate
        -- keys.
        groupSnd :: Ord a => [(a,b)] -> [(a,[b])]
        groupSnd =
            map (\ ps -> (fst (head ps), map snd ps))
            . groupBy ((==) `on` fst)
            . sortBy (compare `on` fst)

        checkEntry (f,ts) =
            case nub ts of
                [_] -> return ()
                ts' ->
                    fail $ "The symbol '" ++ f ++ "' is used at conflicting types:\n" ++
                            unlines (map (("  " ++) . show) ts')

checkDefinition :: Context -> String -> [String] -> Exp -> Err ()
checkDefinition ctx f xs e =
    void $ checkDefinition' dummyConstructors ctx f xs e

data ListConstructors = LC
        { nil   :: Base -> String
        , cons  :: Base -> String
        }

dummyConstructors :: ListConstructors
dummyConstructors = LC (const "[]") (const "(:)")

checkDefinition' :: ListConstructors -> Context -> String -> [String] -> Exp -> Err ([(String,Base)],(Exp,Base))
checkDefinition' list ctx f xs e =
    do  unless (isLower $ head f) $ fail "Defined functions must start with a lowercase letter."
        t@(FunT ts t') <- lookupCtx f ctx `catchErr` \_ ->
                                fail $ "'" ++ f ++ "' must be used in a rule."
        let expect = length ts
            given  = length xs
        unless (expect == given) $ fail $ "'" ++ f ++ "' is used with type " ++ show t ++ " but defined with " ++ show given ++ " argument" ++ plural given ++ "."
        e' <- checkExp list (extendContext ctx $ zip xs (map (FunT []) ts)) e t'
        return (zip xs ts, (e', t'))
    `catchErr` \err -> fail $ "In the definition " ++ unwords (f : xs ++ ["=",show e,";"]) ++ "\n  " ++ err
    where
        plural 1 = ""
        plural _ = "s"

checkExp :: ListConstructors -> Context -> Exp -> Base -> Err Exp
checkExp list _   (App "[]" []) (ListT t) = return (App (nil list t) [])
checkExp _ _      (App "[]" _) _  = fail "[] is applied to too many arguments."
checkExp list ctx (App "(:)" [e,es]) (ListT t) =
    do  e'  <- checkExp list ctx e t
        es' <- checkExp list ctx es (ListT t)
        return $ App (cons list t) [e',es']
checkExp _ _ (App "(:)" es) _   = fail $ "(:) takes 2 arguments, but has been given " ++ show (length es) ++ "."
checkExp list ctx e@(App x es) t =
    do  FunT ts t' <- lookupCtx x ctx
        es' <- matchArgs ts
        unless (t == t') $ fail $ show e ++ " has type " ++ show t' ++ ", but something of type " ++ show t ++ " was expected."
        return $ App x es'
    where
        matchArgs ts
            | expect /= given   = fail $ "'" ++ x ++ "' takes " ++ show expect ++ " arguments, but has been given " ++ show given ++ "."
            | otherwise         = zipWithM (checkExp list ctx) es ts
            where
                expect = length ts
                given  = length es
checkExp _ _ e@(LitInt _) (BaseT "Integer")     = return e
checkExp _ _ e@(LitDouble _) (BaseT "Double")   = return e
checkExp _ _ e@(LitChar _) (BaseT "Char")       = return e
checkExp _ _ e@(LitString _) (BaseT "String")   = return e
checkExp _ _ e t = fail $ show e ++ " does not have type " ++ show t ++ "."
