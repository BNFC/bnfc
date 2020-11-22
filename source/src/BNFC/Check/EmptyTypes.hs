{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Check whether generated AST will have empty types.
--
-- Internal rules are included.
--
-- We compute by a saturation algorithm which token types are used in which non-terminal.
-- A non-terminal does not use any token types, we flag an empty type.

module BNFC.Check.EmptyTypes (emptyData) where

import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import BNFC.CF

-- | Compute the categories that have empty data type declarations in the abstract syntax.
--   Disregards list types.
emptyData :: forall f. (IsFun f) => [Rul f] -> [RCat]
emptyData rs =
  [ pc
  | Rule _ pc _ _ <- rs
  , let c = wpThing pc
  , not $ isList c
  , Left x <- [baseCat c]
  , maybe True List.null $ Map.lookup x ruleMap
  ]
  where
  ruleMap :: Map BaseCat [f]
  ruleMap = Map.unionsWith (++) $ (`mapMaybe` rs) $ \case
    Rule f pc _ _
      | not (isCoercion f), Left x <- baseCat (wpThing pc)
          -> Just $ Map.singleton x [f]
      | otherwise
          -> Nothing


-- -- STILLBORN CODE:

-- type UsedTokenTypes = Map BaseCat (Set TokenCat)

-- -- | Not sure what emptyCats computes:
-- emptyCats :: [Rul f] -> [RCat]
-- emptyCats rs =
--   [ pc
--   | Rule _ pc _ _ <- rs
--   , let c = wpThing pc
--   , not $ isList c
--   , Left x <- [baseCat c]
--   , maybe False Set.null $ Map.lookup x usedTokenMap
--   ]
--   where
--   -- The computation of the UsedTokenTypes is likely correct (but untested).
--   usedTokenMap = saturate Map.empty
--   -- standard least fixed-point iteration from below
--   saturate m = if m' == m then m' else saturate m'
--     where m' = step m
--   -- step is monotone!
--   step :: UsedTokenTypes -> UsedTokenTypes
--   step m = Map.unionsWith Set.union $ map stepRule rs
--     where
--     -- Compute the used tokens for a NT based on a single rule,
--     -- using the information we have already for the NTs on the rhs.
--     stepRule (Rule _ (WithPosition _ c0) rhs _) =
--       case baseCat c0 of
--         Left x -> Map.singleton x $ Set.unions $ map typesFor rhsCats
--         -- The TokenCat case is actually impossible, but this is consistent:
--         Right x -> Map.singleton x $ Set.singleton x
--       where
--       rhsCats = mapMaybe (either (Just . baseCat) (const Nothing)) rhs
--     typesFor = \case
--       -- Not token cat:
--       Left c  -> Map.findWithDefault Set.empty c m
--       -- TokenCat:
--       Right c -> Set.singleton c
