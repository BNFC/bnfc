{-
    BNF Converter: TreeSitter Grammar Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Author        : Kangjing Huang (huangkangjing@gmail.com)
    Created       : 23 Nov, 2023

-}

{-# LANGUAGE LambdaCase #-}

{-|
Description: Identifies and transforms rules which match the empty string,
             as required by Treesitter.
Maintainer: Kait Lam

This module identifies and transforms rules which match the empty string,
as required by constraints of Treesitter.

Treesitter requires that rules do /not/ match the empty string.
Although this is not made explicit in [their documentation](https://tree-sitter.github.io/tree-sitter/creating-parsers/3-writing-the-grammar.html),
rules which match empty will be thoroughly rejected by the tree-sitter
compiler.

For example, this Treesitter grammar is not allowed because @$.listItem@ could match
the empty string.

> list: $ => seq("[", $.listItem, "]"),
> listItem: $ => choice(
>   seq(),
>   "item"
> ),

Instead, Treesitter wants empty matches to be moved to /use-sites/ of that
rule. The above grammar would be rewritten as:

> list: $ => seq("[", optional($.listItem), "]"),
> listItem: $ => choice(
>   choice(),
>   "item"
> ),

Unfortunately, the style Treesitter needs is quite incompatible with LBNF. LBNF
has no way to express "choice" occuring within the right hand side of a rule,
which forces any choice (including potential optionality) to happen at the
top-level of a rule. This is in direct conflict with what Treesitter expects.

This modules bridges the gap by transforming LBNF's rules using process
outlined above. This happens in two steps: first, we compute which rules could
match empty by using a fixpoint algorithm, then, we transform the rules by
eliminating empty matches from all rules and wrapping non-terminals in
@optional@ if their rule could match empty. BNFC's "BNFC.CF" types have no
notion of "optional" within the RHS, so this module also introduces 'OptSym' to
represent this.

Of course, this transformation affects the parse tree for certain strings.
Users of BNFC who want to generate Treesitter grammars should be aware of this
change.

For users of this library, the main functions of interest are in the [Fixpoint
and transformations]("BNFC.Backend.TreeSitter.MatchesEmpty#g:fixpoint")
section.
-}
module BNFC.Backend.TreeSitter.MatchesEmpty where

import BNFC.Utils((>.>))
import BNFC.CF(SentForm, Cat, Rule, rhsRule)

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Set as Set

-- * Basic types

-- | A symbol which is either a non-terminal ('Cat') or terminal token name ('String').
-- A list of these 'Sym's is a sentential form, 'SentForm'.
type Sym = Either Cat String

-- | Set of 'Sym' which are known to match the empty string.
newtype KnownEmpty = KnownEmpty (Set.Set Sym) deriving (Eq, Show)

-- | Returns whether the given symbol matches the empty string, according
-- to the given known empty set.
isKnownEmpty :: Sym -> KnownEmpty -> Bool
isKnownEmpty x ks = x `Set.member` (knownEmptySet ks)

knownEmptySet :: KnownEmpty -> Set.Set Sym
knownEmptySet (KnownEmpty x) = x

-- | Represents a 'Sym' which might be wrapped in a @optional(...)@ function
-- in the produced Treesitter grammar.
data OptSym =
  -- | A 'Sym' which is wrapped in @optional([SYM])@, indicating that
  -- it should match @[SYM]@ /or/ the empty string.
  Optional Sym |
  -- | A plain 'Sym' which matches only the 'Sym' itself.
  NonOptional Sym deriving (Eq, Show)

-- | A sentential form where each symbol may be wrapped in an optional function.
-- Analagous to 'SentForm', but containing 'OptSym' instead of 'Sym'.
type OptSentForm = [OptSym]

-- * "Matches empty" type

-- | Represents whether the wrapped value matches the empty string, or whether
-- it is known to be non-empty.
--
-- Because this analysis is done on context-free grammars, the analysis is
-- precise. A value of 'MatchesEmpty' /will/ accept the empty string, and a
-- value of 'NonEmpty' will not. There is no uncertainty in this analysis.
data MatchesEmpty a =
  -- | The contained value /accepts/ the empty string.
  MatchesEmpty a |
  -- | The contained value /does not/ accept the empty string.
  NonEmpty a deriving (Eq, Show)

matchesEmpty :: MatchesEmpty a -> Bool
matchesEmpty (MatchesEmpty _) = True
matchesEmpty (NonEmpty _) = False

unMatchesEmpty :: MatchesEmpty a -> a
unMatchesEmpty (MatchesEmpty x) = x
unMatchesEmpty (NonEmpty x) = x

-- ** Sequential operators

-- | Combines the two values /in sequence/. Returns v'MatchesEmpty' if both values
-- are v'MatchesEmpty', otherwise returns v'NonEmpty'. In all cases, the inner
-- values are joined using the semigroup operation.
seqMatchesEmpty :: Semigroup a => MatchesEmpty a -> MatchesEmpty a -> MatchesEmpty a
seqMatchesEmpty (MatchesEmpty x) (MatchesEmpty y) = MatchesEmpty (x <> y)
seqMatchesEmpty               x                y  = NonEmpty (unMatchesEmpty x <> unMatchesEmpty y)

-- | Combines the list of values /in sequence/ (i.e., @seq(x1, ..., xn)@), returning
-- v'MatchesEmpty' if all are v'MatchesEmpty', otherwise v'NonEmpty'. Inner values
-- are joined using the semigroup operation.
seqListMatchesEmpty :: Monoid a => [MatchesEmpty a] -> MatchesEmpty a
seqListMatchesEmpty = foldr seqMatchesEmpty (MatchesEmpty mempty)


-- ** Alternation operators

-- | Combines the two values as a /parallel choice/. Returns v'NonEmpty' if
-- both values are v'NonEmpty', otherwise returns v'MatchesEmpty'. In all
-- cases, the inner values are joined using the semigroup operation.
choiceMatchesEmpty :: Semigroup a => MatchesEmpty a -> MatchesEmpty a -> MatchesEmpty a
choiceMatchesEmpty (NonEmpty x) (NonEmpty y) = NonEmpty (x <> y)
choiceMatchesEmpty           x            y  = MatchesEmpty (unMatchesEmpty x <> unMatchesEmpty y)

-- | Combines the list of values /in choice/ (i.e., @choice(x1, ..., xn)@), returning
-- v'NonEmpty' if all are v'NonEmpty', otherwise v'MatchesEmpty'. Inner values
-- are joined using the semigroup operation.
choiceListMatchesEmpty :: Monoid a => [MatchesEmpty a] -> MatchesEmpty a
choiceListMatchesEmpty = foldr choiceMatchesEmpty (NonEmpty mempty)

-- * Analysis of non-terminals

-- | Determines whether the given symbol can match empty, according to the
-- given known empty set. If it /can/ match empty, the symbol is returned as
-- v'Optional' to indicate that uses of the symbol should match empty.
--
-- TODO: This does not yet handle /tokens/ (terminals) which might be empty.
-- At the moment, all terminals are assumed to be non-empty.
possiblyEmptySym :: KnownEmpty -> Sym -> OptSym
possiblyEmptySym knownEmpty sym =
  if sym `isKnownEmpty` knownEmpty then
    Optional sym
  else
    NonOptional sym

-- | Determines whether the given sentential form could match empty.
--
-- The returned list is a /choice/ list of 'OptSentForm', with v'Optional'
-- applied to symbols which are within the known empty set. When combined using
-- choice, the returned list is equivalent to the original rule, /except/ that
-- the returned list has empty matches removed. If the rule previously matched
-- empty, this is encoded as the v'MatchesEmpty' variant.
--
-- __Implementation Detail__: blah
possiblyEmptyRule :: KnownEmpty -> SentForm -> MatchesEmpty [OptSentForm]
possiblyEmptyRule knownEmpty =
  map (possiblyEmptySym knownEmpty)
  >.> map fromOpt
  >.> seqListMatchesEmpty
  >.> \case
    MatchesEmpty sent -> MatchesEmpty (subtractEmptyString sent)
    NonEmpty sent -> NonEmpty [sent]
  where
    fromOpt (Optional x) = MatchesEmpty [Optional x]
    fromOpt (NonOptional x) = NonEmpty [NonOptional x]

    subtractEmptyString = Maybe.mapMaybe headNonOptional . List.tails

    headNonOptional (Optional x : xs) = Just (NonOptional x : xs)
    headNonOptional (NonOptional _ : _) = error "headNonOptional: unexpected that head is already NonOptional"
    headNonOptional [] = Nothing

-- | Determines whether the given non-terminal category with the given
-- production rules could match empty.
--
-- The returned list is a /choice/ list of 'OptSentForm', with v'Optional'
-- applied to symbols which are within the known empty set. When combined using
-- choice, the returned list is equivalent to the original rules, /except/ that
-- the returned list has empty matches removed. If the category previously
-- matched empty, this is encoded as the v'MatchesEmpty' variant.
possiblyEmptyCat :: KnownEmpty -> (Cat, [Rule]) -> MatchesEmpty [OptSentForm]
possiblyEmptyCat knownEmpty (_, rules) =
  choiceListMatchesEmpty $ map (possiblyEmptyRule knownEmpty . rhsRule) rules

-- | Updates the set of known empty symbols according to the given grammar.
-- Returns the new set, which is made up of the previous set unioned with any
-- newly-discovered empty matching symbols.
--
-- This is one step of the fixpoint computation in 'fixPointKnownEmpty'.
possiblyEmptyCats :: [(Cat, [Rule])] -> KnownEmpty -> KnownEmpty
possiblyEmptyCats cats knownEmpty =
  KnownEmpty $
    Set.fromList (map (Left . fst) newEmptyCats)
      `Set.union` knownEmptySet knownEmpty
  where
    newEmptyCats = filter (matchesEmpty . possiblyEmptyCat knownEmpty) cats

-- * Fixpoint and transformations #fixpoint#
--
-- $fixpoint
-- For users of this module, these are the main functions of interest.

-- | Computes the complete set of symbols which are known to match empty,
-- using the given non-terminal production rules.
--
-- This should be given the list of parsable grammar rules, e.g., from
-- 'BNFC.CF.ruleGroups.
fixPointKnownEmpty :: [(Cat, [Rule])] -> KnownEmpty
fixPointKnownEmpty cats = go (KnownEmpty Set.empty)
  where
    step = possiblyEmptyCats cats

    go x = if x == x' then x else go x'
      where x' = step x

-- | Transforms the given sentence such that the returned sentential form does
-- not match the empty string, and contains v'Optional' terms where needed.
--
-- The returned list is a /choice/ list which is equivalent to the given
-- sentential form, but for the (potential) subtraction of empty matches.
--
-- v'Optional' is inserted around symbols which previously matched the empty
-- string (according to the given 'KnownEmpty'). This compensates for
-- v'transformEmptyMatches' being applied to /other/ rules of the grammar.
--
-- After this transformation is applied to all rules of the grammar, the
-- grammar should accept an identical language. However, the exact nodes which
-- match certain strings might change.
transformEmptyMatches :: KnownEmpty -> SentForm -> [OptSentForm]
transformEmptyMatches knownEmpty = unMatchesEmpty . possiblyEmptyRule knownEmpty

