{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}

-- | Tools to manipulate regular expressions.

module BNFC.Regex ( simpReg ) where

import Data.Monoid    (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

import AbsBNF

-- | Simplification of regular expression, mostly for the purpose
--   of simplifying character alternatives (character classes).
--
--   This may help lexer backends, since often lexer generators
--   have a limited syntax for character classes.
--
simpReg :: Reg -> Reg
simpReg = rloop
  where
  rloop = rx . loop
  loop :: Reg -> RC
  loop = \case
    -- Definitely not character classes:
    -- RSeq   r1 r2 -> Rx $ rloop r1 `rSeq` rloop r2
    RStar  r     -> Rx $ rStar $ rloop r
    RPlus  r     -> Rx $ rPlus $ rloop r
    ROpt   r     -> Rx $ rOpt  $ rloop r
    REps         -> Rx $ REps
    RSeqs ""     -> Rx $ REps
    RSeqs [c]    -> CC $ cChar c
    RSeqs s      -> Rx $ RSeqs s
    -- Possibly character classes:
    RSeq   r1 r2 -> loop r1 `rcSeq`   loop r2
    RAlt   r1 r2 -> loop r1 `rcAlt`   loop r2
    RMinus r1 r2 -> loop r1 `rcMinus` loop r2
    -- Definitely character classes:
    RChar c      -> CC $ cChar c
    RAlts s      -> CC $ cAlts s
    RDigit       -> CC $ cDigit
    RLetter      -> CC $ cLetter
    RUpper       -> CC $ cUpper
    RLower       -> CC $ cLower
    RAny         -> CC $ cAny


-- | Character classes are regular expressions that recognize
-- character sequences of length exactly one.  These are often
-- distinguished from arbitrary regular expressions in lexer
-- generators, e.g. in @alex@.
--
-- We represent character classes as a difference of unions of atomic
-- character classes.

data CharClass = CMinus { ccYes, ccNo :: CharClassUnion }
  deriving (Eq, Ord, Show)

data CharClassUnion
  = CAny                       -- ^ Any character.
  | CAlt (Set CharClassAtom)  -- ^ Any of the given (≥0) alternatives.
  deriving (Eq, Ord, Show)

data CharClassAtom
  = CChar Char  -- ^ A single character.
  | CDigit      -- ^ @0-9@.
  | CLower      -- ^ Lower case character.
  | CUpper      -- ^ Upper case character.
  deriving (Eq, Ord, Show)

-- -- | Regular expressions are constructed over character classes.
-- --
-- --   We do not simplify at the level of regular expressions;
-- --   this is left to the backend.
-- data Regex
--     = RxAlt Reg Reg
--     | RxMinus Reg Reg
--     | RxSeq Reg Reg
--     | RxStar Reg
--     | RxPlus Reg
--     | RxOpt Reg
--     | RxEps
--     | RxChar CharClass
--   deriving (Eq, Ord, Show)

-- | A regular expression that might be a character class.
data RC
  = Rx Reg
  | CC CharClass

-- * Smart constructors for regular expressions.

rSeq :: Reg -> Reg -> Reg
rSeq = curry $ \case
  -- 0r = 0
  (RAlts "", _       ) -> RAlts ""
  (_       , RAlts "") -> RAlts ""
  -- 1r = r
  (REps    , r       ) -> r
  (RSeqs "", r       ) -> r
  (r       , REps    ) -> r
  (r       , RSeqs "") -> r
  -- r*r* = r*
  (RStar r1, RStar r2) | r1 == r2 -> rStar r1
  -- r+r* = r*r+ = r+
  (RPlus r1, RStar r2) | r1 == r2 -> rPlus r1
  (RStar r1, RPlus r2) | r1 == r2 -> rPlus r1
  -- rr* = r*r = r+
  (r1      , RStar r2) | r1 == r2 -> rPlus r1
  (RStar r1, r2      ) | r1 == r2 -> rPlus r1
  -- character sequences
  (RSeqs s1, RSeqs s2) -> RSeqs $ s1 ++ s2
  (RChar c1, RSeqs s2) -> RSeqs $ c1 : s2
  (RSeqs s1, RChar c2) -> RSeqs $ s1 ++ [c2]
  (RChar c1, RChar c2) -> RSeqs [ c1, c2 ]
  -- Associate to the left
  (r1    , RSeq r2 r3) -> (r1 `rSeq` r2) `rSeq` r3
  -- general sequences
  (r1      , r2      ) -> r1 `RSeq` r2

rAlt :: Reg -> Reg -> Reg
rAlt = curry $ \case
  -- 0 + r = r
  (RAlts "", r       ) -> r
  -- r + 0 = r
  (r       , RAlts "") -> r
  -- join character alternatives
  (RAlts s1, RAlts s2) -> RAlts $ s1 ++ s2
  (RChar c1, RAlts s2) -> RAlts $ c1 : s2
  (RAlts s1, RChar c2) -> RAlts $ s1 ++ [c2]
  (RChar c1, RChar c2) -> RAlts [ c1, c2 ]
  -- Associate to the left
  (r1    , RAlt r2 r3) -> (r1 `rAlt` r2) `rAlt` r3
  -- general alternatives
  (r1, r2)
     | r1 == r2  -> r1  -- idempotency, but not the general case
     | otherwise -> r1 `RAlt` r2

rMinus :: Reg -> Reg -> Reg
rMinus = curry $ \case
  -- 0 - r = 0
  (RAlts "", _       ) -> RAlts ""
  -- r - 0 = r
  (r       , RAlts "") -> r
  -- join character alternatives
  (RAlts s1, RAlts s2) -> case s1 List.\\ s2 of
    [c] -> RChar c
    s   -> RAlts s
  (r1, r2)
     | r1 == r2  -> RAlts ""
     | otherwise -> r1 `RMinus` r2

rStar :: Reg -> Reg
rStar = \case
  REps     -> REps
  RSeqs "" -> REps
  RAlts "" -> REps
  ROpt  r  -> RStar r
  RStar r  -> RStar r
  RPlus r  -> RStar r
  r        -> RStar r

rPlus :: Reg -> Reg
rPlus = \case
  REps     -> REps
  RSeqs "" -> REps
  RAlts "" -> RAlts ""
  ROpt  r  -> RStar r
  RStar r  -> RStar r
  RPlus r  -> RPlus r
  r        -> RPlus r

rOpt :: Reg -> Reg
rOpt = \case
  REps     -> REps
  RSeqs "" -> REps
  RAlts "" -> REps
  RStar r  -> RStar r
  RPlus r  -> RStar r
  ROpt  r  -> ROpt  r
  r        -> ROpt  r

rcSeq :: RC -> RC -> RC
rcSeq = curry $ \case
  (Rx REps      , r            ) -> r
  (Rx (RSeqs ""), r            ) -> r
  (r            , Rx REps      ) -> r
  (r            , Rx (RSeqs "")) -> r
  (r1           , r2           ) -> Rx $ rx r1 `rSeq` rx r2

rcAlt :: RC -> RC -> RC
rcAlt = curry $ \case
  -- 0 + r = r + 0 = r
  (Rx (RAlts ""), r) -> r
  (r, Rx (RAlts "")) -> r
  -- other cases
  (CC c1, CC c2) -> c1 `cAlt` c2
  (c1   , c2   ) -> Rx $ rx c1 `rAlt` rx c2

rcMinus :: RC -> RC -> RC
rcMinus = curry $ \case
  -- r - 0 = r
  (r    , Rx (RAlts "")) -> r
  (CC c1, CC c2        ) -> c1 `cMinus` c2
  (c1   , c2           ) -> Rx $ rx c1 `rMinus` rx c2

class ToReg a where
  rx :: a -> Reg

instance ToReg RC where
  rx (Rx r) = r
  rx (CC c) = rx c

instance ToReg CharClass where
  rx (CMinus p m)
    | m == mempty = rx p
    | p == mempty = RAlts ""
    | otherwise = rx p `RMinus` rx m

instance ToReg CharClassUnion where
  rx CAny      = RAny
  rx (CAlt cs) = case rs of
      []  -> RAlts ""
      [r] -> r
      rs  -> foldr1 RAlt rs
    where
    -- collect elements of cs into St
    start = St False False False ""
    step st = \case
      CChar c -> st { stAlts = c : stAlts st }
      CDigit  -> st { stDigit = True }
      CLower  -> st { stLower = True }
      CUpper  -> st { stUpper = True }
    (St digit upper lower alts) = foldl step start $ Set.toDescList cs
    rs = concat
      [ [ RChar c    | [c] <- [alts]      ]
      , [ RAlts alts | (_:_:_) <- [alts]  ]
      , [ RDigit     | digit              ]
      , [ RLetter    | upper && lower     ]
      , [ RUpper     | upper && not lower ]
      , [ RLower     | lower && not upper ]
      ]

  -- Local state type
data St = St { stDigit, stUpper, stLower :: Bool, stAlts :: String }

-- UNUSED:
-- instance ToReg CharClassAtom where
--   rx = \case
--     CChar c -> RChar c
--     CDigit  -> RDigit
--     CLower  -> RLower
--     CUpper  -> RUpper

-- * Constructors for character classes.

-- |
-- @(p1 \ m1) ∪ (p2 \ m2) = (p1 ∪ p2) \ (m1 ∪ m2)@ if @p1 ⊥ m2@ and @p2 ⊥ m1@
cAlt :: CharClass -> CharClass -> RC
cAlt c1@(CMinus p1 m1) c2@(CMinus p2 m2)
  | c1 == cAny || c2 == cAny     = CC cAny
  | p1 `ccuMinus` m2 == Right p1,
    p2 `ccuMinus` m1 == Right p2 = CC $ either id ccu $ (p1 <> p2) `ccuMinus` (m1 <> m2)
  | otherwise                    = Rx $ rx c1 `RAlt` rx c2

  -- -- | ccuDisjoint p1 m2, ccuDisjoint p2 m1 = CC $ either id ccu $ (p1 <> p2) `ccuMinus` (m1 <> m2)
  -- -- | null m1, null m2 = CC $ ccu (p1 <> p2)

-- |
-- @(p1 \ m1) \ (0 \ m2)  = p1 \ m1@
-- @(p1 \ m1) \ (p2 \ m2) = p1 \ (m1 ∪ p2)@  if @p1 \ m2 = p1@
cMinus :: CharClass -> CharClass -> RC
cMinus c1@(CMinus p1 m1) c2@(CMinus p2 m2)
  | p2 == mempty                 = CC c1
  | p1 `ccuMinus` m2 == Right p1 = CC $ either id ccu $ p1 `ccuMinus` (m1 <> p2)
  | otherwise                    = Rx $ rx c1 `RMinus` rx c2

cChar :: Char -> CharClass
cChar c = cAlts [c]

cAlts :: String -> CharClass
cAlts cs = ccu $ CAlt $ Set.fromList $ map CChar cs

cDigit, cLower, cUpper, cLetter, cAny :: CharClass
cDigit  = cAtom CDigit
cLower  = cAtom CLower
cUpper  = cAtom CUpper
cLetter = ccu $ CAlt $ Set.fromList [ CLower, CUpper ]
cAny    = ccu CAny

cAtom :: CharClassAtom -> CharClass
cAtom = ccu . CAlt . Set.singleton

ccu :: CharClassUnion -> CharClass
ccu = (`CMinus` mempty)

-- (A \ B) \ (C \ D) = A \ (B ∪ (C \ D))

-- | Mutually reduce:  @(A - B) = (A \ B) - (B \ A)@
ccuMinus :: CharClassUnion -> CharClassUnion -> Either CharClass CharClassUnion
ccuMinus = curry $ \case
  (_      , CAny)   -> Right mempty
  (c1@CAny, c2  )
    | c2 == mempty  -> Right $ c1
    | otherwise     -> Left  $ c1 `CMinus` c2
  (CAlt cs1, CAlt cs2)
    | Set.null cs1' ||
      Set.null cs2' -> Right $ CAlt cs1'
    | otherwise     -> Left  $ CAlt cs1' `CMinus` CAlt cs2'
    where
    cs1' = cs1 Set.\\ cs2
    cs2' = cs2 Set.\\ cs1

instance Semigroup CharClassUnion where
  CAny    <> _        = CAny
  _       <> CAny     = CAny
  CAlt cs <> CAlt cs' = CAlt (cs <> cs')

instance Monoid CharClassUnion where
  mempty  = CAlt Set.empty
  mappend = (<>)
