{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

-- | Type checker for defined syntax constructors @define f xs = e@.

module BNFC.TypeChecker
  ( -- * Type checker entry point
    runTypeChecker
  , checkDefinitions
    -- * Backdoor for rechecking defined syntax constructors for list types
  , checkDefinition'
  , buildSignature, buildContext, ctxTokens, isToken
  , ListConstructors(..)
  ) where

import Control.Monad
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader

import Data.Bifunctor
import Data.Char
import Data.Either (partitionEithers)

import qualified Data.Map as Map
import qualified Data.Set as Set

import BNFC.CF
import BNFC.PrettyPrint

-- * Error monad

type TCError = WithPosition String

-- | Type checking monad, reports errors.
newtype Err a = Err { unErr :: ReaderT Position (Either TCError) a }
  deriving (Functor, Applicative, Monad, MonadReader Position)

instance MonadError String Err where
  throwError msg = Err $ do
    pos <- ask
    throwError $ WithPosition pos msg
  catchError m h = Err $ do
    unErr m `catchError` \ (WithPosition _ msg) -> unErr (h msg)

withPosition :: Position -> Err a -> Err a
withPosition pos = local (const pos)

runTypeChecker :: Err a -> Either String a
runTypeChecker m = first blendInPosition $ unErr m `runReaderT` NoPosition

-- * Types and context

data Context = Ctx
  { ctxLabels :: Signature         -- ^ Types of labels, extracted from rules.
  , ctxTokens :: [String]          -- ^ User-defined token types.
  , ctxLocals :: Telescope         -- ^ Types of local variables of a definition.
  }

data ListConstructors = LC
  { nil   :: Base -> (String, Type)  -- ^ 'Base' is the element type. 'Type' the list type.
  , cons  :: Base -> (String, Type)
  }

dummyConstructors :: ListConstructors
dummyConstructors = LC
  { nil  = \ b -> ("[]" , FunT [] (ListT b))
  , cons = \ b -> ("(:)", FunT [b, ListT b] (ListT b))
  }

-- * Type checker for definitions and expressions

-- | Entry point.
checkDefinitions :: CF -> Err CF
checkDefinitions cf = do
  let ctx = buildContext cf
  let (pragmas, defs0) = partitionEithers $ map isFunDef $ cfgPragmas cf
  defs <- mapM (checkDefinition ctx) defs0
  return cf { cfgPragmas = pragmas ++ map FunDef defs }

checkDefinition :: Context -> Define -> Err Define
checkDefinition ctx (Define f args e0 _) = do
  let xs = map fst args  -- Throw away dummy types.
  (tel, (e, b)) <- checkDefinition' dummyConstructors ctx f xs e0
  return $ Define f tel e b

checkDefinition'
  :: ListConstructors  -- ^ Translation of the list constructors.
  -> Context           -- ^ Signature (types of labels).
  -> RFun              -- ^ Function name.
  -> [String]          -- ^ Function arguments.
  -> Exp               -- ^ Function body.
  -> Err (Telescope, (Exp, Base))  -- ^ Typed arguments, translated body, type of body.
checkDefinition' list ctx ident xs e =
  withPosition (wpPosition ident) $
    do  unless (isLower $ head f) $ throwError $
          "Defined functions must start with a lowercase letter."
        t@(FunT ts t') <- lookupCtx f ctx `catchError` \_ ->
                                throwError $ "'" ++ f ++ "' must be used in a rule."
        let expect = length ts
            given  = length xs
        unless (expect == given) $ throwError $ concat
          [ "'", f, "' is used with type ", show t
          , " but defined with ", show given, " argument", plural given ++ "."
          ]
        e' <- checkExp list (setLocals ctx $ zip xs ts) e t'
        return (zip xs ts, (e', t'))
    `catchError` \ err -> throwError $
      "In the definition " ++ unwords (f : xs ++ ["=", prettyShow e, ";"]) ++ "\n  " ++ err
    where
        f = wpThing ident
        plural 1 = ""
        plural _ = "s"

checkExp :: ListConstructors -> Context -> Exp -> Base -> Err Exp
checkExp list ctx = curry $ \case
  (App "[]" _ []     , ListT t      ) -> return (uncurry App (nil list t) [])
  (App "[]" _ _      , _            ) -> throwError $
    "[] is applied to too many arguments."

  (App "(:)" _ [e,es], ListT t      ) -> do
    e'  <- checkExp list ctx e t
    es' <- checkExp list ctx es (ListT t)
    return $ uncurry App (cons list t) [e',es']

  (App "(:)" _ es  , _              ) -> throwError $
    "(:) takes 2 arguments, but has been given " ++ show (length es) ++ "."

  (e@(App x _ es)  , t              ) -> checkApp e x es t
  (e@(Var x)       , t              ) -> e <$ checkApp e x [] t
  (e@LitInt{}      , BaseT "Integer") -> return e
  (e@LitDouble{}   , BaseT "Double" ) -> return e
  (e@LitChar{}     , BaseT "Char"   ) -> return e
  (e@LitString{}   , BaseT "String" ) -> return e
  (e               , t              ) -> throwError $
    prettyShow e ++ " does not have type " ++ show t ++ "."
  where
  checkApp e x es t = do
    ft@(FunT ts t') <- lookupCtx x ctx
    es' <- matchArgs ts
    unless (t == t') $ throwError $ prettyShow e ++ " has type " ++ show t' ++ ", but something of type " ++ show t ++ " was expected."
    return $ App x ft es'
    where
    matchArgs ts
      | expect /= given   = throwError $ "'" ++ x ++ "' takes " ++ show expect ++ " arguments, but has been given " ++ show given ++ "."
      | otherwise         = zipWithM (checkExp list ctx) es ts
      where
        expect = length ts
        given  = length es

-- * Context handling

-- | Create context containing the types of all labels,
--   computed from the rules.
--
--   Fail if a label is used at different types.
--
buildSignature :: [Rule] -> Err Signature
buildSignature rules = do
  -- Build label signature with duplicates
  let sig0 = Map.fromListWith mappend $ map (second Set.singleton) labels
  -- Check for duplicates; extract from singleton sets.
  sig <- forM (Map.toAscList sig0) $ \ (f,ts) ->
    case Set.toList ts of
      []  -> undefined  -- impossible
      [t] -> return (f,t)
      ts' -> throwError $ unlines $ concat
        [ [ "The label '" ++ f ++ "' is used at conflicting types:" ]
        , map (("  " ++) . blendInPosition . fmap show) ts'
        ]
  return $ Map.fromAscList sig
  where
    mkType cat args = FunT [ mkBase t | Left t <- args ]
                           (mkBase cat)
    mkBase t
        | isList t  = ListT $ mkBase $ normCatOfList t
        | otherwise = BaseT $ catToStr $ normCat t

    labels =
      [ (x, WithPosition pos $ mkType (wpThing cat) args)
        | Rule f@(WithPosition pos x) cat args _ <- rules
        , not (isCoercion f)
        , not (isNilCons f)
      ]

buildContext :: CF -> Context
buildContext cf = Ctx
    { ctxLabels = cfgSignature cf
    , ctxTokens = ("Ident" : tokenNames cf)
    , ctxLocals = []
    }

isToken :: String -> Context -> Bool
isToken x ctx = elem x $ ctxTokens ctx

setLocals :: Context -> [(String,Base)] -> Context
setLocals ctx xs = ctx { ctxLocals = xs }

lookupCtx :: String -> Context -> Err Type
lookupCtx x ctx
  | isToken x ctx = return $ FunT [BaseT "String"] (BaseT x)
  | otherwise     = do
    case lookup x $ ctxLocals ctx of
      Just b -> return $ FunT [] b
      Nothing -> do
        case Map.lookup x $ ctxLabels ctx of
          Nothing -> throwError $ "Undefined symbol '" ++ x ++ "'."
          Just t  -> return $ wpThing t
