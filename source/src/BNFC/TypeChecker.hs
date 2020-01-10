{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Type checker for defined syntax constructors @define f xs = e@.

module BNFC.TypeChecker
  ( -- * Type checker entry point
    checkDefinitions
  , Base(..)
  , -- * Backdoor for rechecking defined syntax constructors for list types
    checkDefinition'
  , buildSignature, buildContext, ctxTokens, isToken
  , ListConstructors(LC)
  ) where

import Control.Monad
import Control.Monad.Except (MonadError(..))

import Data.Bifunctor (second)
import Data.Char

import qualified Data.Map as Map
import qualified Data.Set as Set

import BNFC.CF

-- * Types and context

-- | Type checking monad, reports errors.
type Err = Either String

-- | Function arguments with type.
type Telescope = [(String, Base)]

data Context = Ctx
  { ctxLabels :: Signature         -- ^ Types of labels, extracted from rules.
  , ctxTokens :: [String]          -- ^ User-defined token types.
  , ctxLocals :: Telescope         -- ^ Types of local variables of a definition.
  }

data ListConstructors = LC
        { nil   :: Base -> String
        , cons  :: Base -> String
        }

dummyConstructors :: ListConstructors
dummyConstructors = LC (const "[]") (const "(:)")


-- * Type checker for definitions and expressions

-- | Entry point.
checkDefinitions :: CF -> Err ()
checkDefinitions cf = do
  let ctx = buildContext cf
  sequence_ [ checkDefinition ctx f xs e | FunDef f xs e <- cfgPragmas cf ]

checkDefinition :: Context -> String -> [String] -> Exp -> Err ()
checkDefinition ctx f xs e =
    void $ checkDefinition' dummyConstructors ctx f xs e

checkDefinition'
  :: ListConstructors  -- ^ Translation of the list constructors.
  -> Context           -- ^ Signature (types of labels).
  -> String            -- ^ Function name.
  -> [String]          -- ^ Function arguments.
  -> Exp               -- ^ Function body.
  -> Err (Telescope, (Exp, Base))  -- ^ Typed arguments, translated body, type of body.
checkDefinition' list ctx f xs e =
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
      "In the definition " ++ unwords (f : xs ++ ["=",show e,";"]) ++ "\n  " ++ err
    where
        plural 1 = ""
        plural _ = "s"

checkExp :: ListConstructors -> Context -> Exp -> Base -> Err Exp
checkExp list ctx = curry $ \case
  (App "[]" []     , ListT t        ) -> return (App (nil list t) [])
  (App "[]" _      , _              ) -> throwError $
    "[] is applied to too many arguments."

  (App "(:)" [e,es], ListT t        ) -> do
    e'  <- checkExp list ctx e t
    es' <- checkExp list ctx es (ListT t)
    return $ App (cons list t) [e',es']

  (App "(:)" es    , _              ) -> throwError $
    "(:) takes 2 arguments, but has been given " ++ show (length es) ++ "."

  (e@(App x es)    , t              ) -> checkApp e x es t
  (e@(Var x)       , t              ) -> e <$ checkApp e x [] t
  (e@LitInt{}      , BaseT "Integer") -> return e
  (e@LitDouble{}   , BaseT "Double" ) -> return e
  (e@LitChar{}     , BaseT "Char"   ) -> return e
  (e@LitString{}   , BaseT "String" ) -> return e
  (e               , t              ) -> throwError $
    show e ++ " does not have type " ++ show t ++ "."
  where
  checkApp e x es t = do
    FunT ts t' <- lookupCtx x ctx
    es' <- matchArgs ts
    unless (t == t') $ throwError $ show e ++ " has type " ++ show t' ++ ", but something of type " ++ show t ++ " was expected."
    return $ App x es'
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
        , map (("  " ++) . show) ts'
        ]
  return $ Map.fromAscList sig
  where
    mkType cat args = FunT [ mkBase t | Left t <- args ]
                           (mkBase cat)
    mkBase t
        | isList t  = ListT $ mkBase $ normCatOfList t
        | otherwise = BaseT $ catToStr $ normCat t

    labels =
      [ (f, mkType cat args)
        | Rule f cat args _ <- rules
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
          Just t  -> return t
