{-|
  Module      : BNFC.Backend.CPPVar.FlexRegex
  Description : FLex regular expressions manipulator.

  FLex regular expressions manipulator.
-}

module BNFC.Backend.CPPVar.FlexRegex
  (
    -- * The regular expression data structure
    FlexRegex(..)

    -- * 'FlexRegex' smart constructors
  , flexCharsetUTF8
  , flexConcat
  , flexStringUTF8
  , byteset
  , flexOr
  , onebyte

    -- * 'BNFC.RegexMinus.SimpleRegex' UTF-8 smart constructors
  , simpleCharsetUTF8
  , simpleStringUTF8

    -- * Conversion
  , fromSimpleRegex
  , fromBNFCReg
  , byte2char

    -- * Precedence information
  , flexRegexPrecedence
  , precedenceEmpty
  , precedenceOnebyte
  , precedenceByteset
  , precedenceStar
  , precedenceOptional
  , precedencePlus
  , precedenceConcat
  , precedenceOr
  ) where

-- Language imports
import Prelude hiding ((<>))
import Data.Char
import Numeric
import Data.Either
import Data.List (delete, partition)
import Data.Int (Int8)
import qualified Data.Set as Set
import Data.Set (Set)

import Text.PrettyPrint (text)

-- BNFC imports
import qualified BNFC.Abs
import BNFC.PrettyPrint ((<>), Pretty (pretty, prettyPrec))
import qualified BNFC.RegexMinus as Minus

import BNFC.Backend.CPPVar.CPPUtil (utf8encode)

------------------------------------------------------------------------
-- * The FlexRegex data structure and its smart constructors.
------------------------------------------------------------------------

-- | The FLex-specific regex representation. Does not represent some features
-- (e.g. character class subtraction) for simplicity.
--
-- The empty language is represented as a v'Byteset' of an empty set.
data FlexRegex
  = Empty                 -- ^ Matches the empty string.
  | Onebyte  !Int8        -- ^ Matches exactly one specified byte.
  | Byteset  !(Set Int8)  -- ^ Matches any one of the bytes.
  | Star     !FlexRegex   -- ^ The Kleene star (matches 0 or more of the regex).
  | Optional !FlexRegex   -- ^ Matches the empty string or the regex.
  | Plus     !FlexRegex   -- ^ Matches 1 or more of the regex.
  | Concat   !FlexRegex !FlexRegex  -- ^ Sequence of two regexes.
  | Or       !FlexRegex !FlexRegex  -- ^ Matches either one of the two regexes.
  deriving (Show, Ord, Eq)

-- | Creates a v'Onebyte' 'FlexRegex' that matches the given /single-byte/
-- character.
--
-- This function throws if the given character's 'ord' is more than 255.
onebyte :: Char -> FlexRegex
onebyte ch = Onebyte o
  where
    _o = ord ch
    o
      | 0 <= _o && _o <= 255 = fromIntegral _o
      | otherwise            = error "onebyte called with a non-byte character"

-- | Creates a v'Byteset' 'FlexRegex' that matches any character from the given
-- string of /single-byte/ characters.
--
-- This function throws if any one of the given characters' 'ord' is more than
-- 255. For a function that automatically encodes wide characters into UTF-8,
-- see 'flexCharsetUTF8'.
byteset ::
     String  -- ^ The list of /single-byte/ characters to match.
  -> FlexRegex
byteset s = Byteset $ Set.fromList ords
  where
    _ords = map ord s
    ords
      | all (\ i -> 0 <= i && i <= 255) _ords = map fromIntegral _ords
      | otherwise = error "byteset called with a non-byte-character string"

-- | Concatenates multiple regexes. As a special case, returns v'Empty' if
-- provided an empty list.
flexConcat :: [FlexRegex] -> FlexRegex
flexConcat = \case
  []       -> Empty
  nonempty -> foldr1 Concat nonempty

-- | Creates a 'FlexRegex' that matches any of the provided regexes.
-- As a special case, returns an empty v'Byteset' if provided an empty list.
flexOr :: [FlexRegex] -> FlexRegex
flexOr = \case
  []       -> Byteset $ Set.fromList []
  nonempty -> foldr1 Or nonempty

-- | Encodes the string with UTF-8 and makes a 'FlexRegex' that matches this
-- encoding.
flexStringUTF8 :: String -> FlexRegex
flexStringUTF8 = flexConcat . map Onebyte . concatMap (utf8encode . ord)

-- | Encodes each character of the string with UTF-8 and makes a 'FlexRegex'
-- that matches any one of the obtained byte sequences.
flexCharsetUTF8 :: String -> FlexRegex
flexCharsetUTF8 = flexOr . map (flexConcat . map Onebyte . utf8encode . ord)

-- | Encodes the string with UTF-8 and makes a 'Minus.SimpleRegex' over 'Int8'
-- that matches this encoding.
simpleStringUTF8 :: String -> Minus.SimpleRegex Int8
simpleStringUTF8 = Minus.string . concatMap (utf8encode . ord)

-- | Encodes each character of the string with UTF-8 and makes a
-- 'BNFC.RegexMinus.SimpleRegex' over @Int8@ that matches any one of the
-- obtained byte sequences.
simpleCharsetUTF8 :: String -> Minus.SimpleRegex Int8
simpleCharsetUTF8 =
  foldr Minus.Or Minus.Phi . map (Minus.string . utf8encode . ord)

------------------------------------------------------------------------
-- * Visualization.
------------------------------------------------------------------------

-- | Visualizes the 'FlexRegex' as a 'BNFC.PrettyPrint.Doc'
-- to be used in a FLex grammar.
instance Pretty FlexRegex where
  pretty = \case
      Empty          -> text "\"\""
      Onebyte byte   -> text $ byte2char False byte
      Byteset set    -> text $ displayByteset (Set.toList set)
      Star regex     -> prettyPrec precedenceStar regex <> text "*"
      Optional regex -> prettyPrec precedenceOptional regex <> text "?"
      Plus regex     -> prettyPrec precedencePlus regex <> text "+"
      Concat r1 r2   -> prettyPrec precedenceConcat r1
                        <> prettyPrec precedenceConcat r2
      Or r1 r2       -> prettyPrec precedenceOr r1 <> text "|"
                        <> prettyPrec precedenceOr r2
    where
      displayByteset = \case
        [ch]  -> byte2char False ch
        chars -> bytecharClass chars

  prettyPrec prec regex
    | flexRegexPrecedence regex < prec = text "(" <> s <> text ")"
    | otherwise                        = s
    where s = pretty regex

-- | Returns a hexadecimal representation of the given byte. Left-pads with
-- zeros to two digits.
hexByte :: Int8 -> String
hexByte byte = leftPad $ showHex unsignedInt ""
  where
    signedInt = fromIntegral byte :: Int
    unsignedInt
      | signedInt < 0 = 256 + signedInt
      | otherwise     = signedInt
    leftPad s = case length s of
      0 -> "00"
      1 -> '0' : s
      _ -> s

-- | Returns a FLex representation of a byte.
byte2char ::
     Bool  -- ^ Are we inside a character class?
  -> Int8  -- ^ The character code.
  -> String
byte2char isInCharclass byte
  | byte == 7  = "\\a"  -- bell/alarm
  | byte == 8  = "\\b"  -- backspace
  | byte == 12 = "\\f" -- form feed
  | byte == 10 = "\\n"
  | byte == 13 = "\\r"
  | byte == 9  = "\\t"
  | byte == 11 = "\\v" -- vertical tab
  | byte `elem` safeBytes = [chr (fromIntegral byte)]
  | isInCharclass && byte `elem` safeInCharclass = [chr (fromIntegral byte)]
  | not isInCharclass && byte `elem` escapedOutOfCharclass
    = ['\\', chr (fromIntegral byte)]
  | otherwise = "\\x" ++ hexByte byte
  where
    safeBytes :: Set Int8
    safeBytes = Set.fromList . map (fromIntegral . ord) $
      ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_@#`'~&%=;"
    safeInCharclass :: Set Int8
    safeInCharclass = Set.fromList . map (fromIntegral . ord) $
      ".+?*<>(){}$/:;\"|"
    escapedOutOfCharclass :: Set Int8
    escapedOutOfCharclass = Set.fromList . map (fromIntegral . ord) $
      ".+?*<>(){}[]$^/:\"\\"

-- | Returns a FLex character class representation.
-- Decides between @[^...]@ and @[...]@, returns whichever is shorter.
--
-- __REQUIRES__ a sorted list as input. The sorted-ness is not checked.
bytecharClass ::
     [Int8]  -- ^ A __sorted__ list of signed bytes.
  -> String
bytecharClass [] = "[^\\0-\\xff]"
bytecharClass bytes
  | bytes == [-128..127] = "[\\0-\\xff]"
  | otherwise = let
      (intsNeg, intsPos) = partition (< 0) $ map fromIntegral bytes
        :: ([Int], [Int])
      ints   = intsPos ++ map (+ 256) intsNeg
      normal = bytecharRanges ints
      inv    = bytecharRanges $ invertByteset 0 ints
    in
      if length normal <= length inv + 1
      then "[" ++ normal ++ "]"
      else "[^" ++ inv ++ "]"
    where
      invertByteset :: Int -> [Int] -> [Int]
      invertByteset 256 _  = []
      invertByteset i orig =
        let inext = i + 1
        in case orig of
          []              -> [i..255]
          curorig : orig' ->
            if i == curorig
            then invertByteset inext orig'
            else i : invertByteset inext orig

-- | Encodes a set of bytes as a union of ranges in a FLex character class.
-- Does not enclose the result in brackets.
--
-- E.g.
--
-- @bytecharRanges ([ord '1'..ord '9'] ++ [ord \'h\'])@
--
-- gives
--
-- @"1-9h"@.
--
-- FLex discourages ranging over different kinds of characters, so we cannot
-- return ranges like @[A-z]@. This is taken into account.
--
-- __REQUIRES__ a sorted list as input. The sorted-ness is not checked.
bytecharRanges ::
     [Int]  -- ^ A __sorted__ list of numbers from 0 to 255.
  -> String
bytecharRanges bytes = concatMap saferanges [digits, caps, lows]
  ++ concatMap (byte2char True . fromIntegral) (bad1 ++ bad2 ++ bad3 ++ bad4)
  ++ escranges nonascii
  where
    (bad1, tail1)    = span (< ord '0')  bytes
    (digits, tail2)  = span (<= ord '9') tail1
    (bad2, tail3)    = span (< ord 'A')  tail2
    (caps, tail4)    = span (<= ord 'Z') tail3
    (bad3, tail5)    = span (< ord 'a')  tail4
    (lows, tail6)    = span (<= ord 'z') tail5
    (bad4, nonascii) = span (<= 127)     tail6
    saferanges :: [Int] -> String
    saferanges = concatMap (uncurry range2str) . getranges
      where
        range2str start end
          | start == end     = [chr start]
          | start + 1 == end = [chr start, chr end]
          | otherwise        = [chr start, '-', chr end]
    escranges = concatMap (uncurry escRange2str) . getranges
      where
        escRange2str start end
          | start == end     = escHex start
          | start + 1 == end = concatMap escHex [start, end]
          | otherwise        = escHex start ++ ('-' : escHex end)
        escHex :: Int -> String
        escHex = ("\\x" ++) . hexByte . fromIntegral
    getranges :: [Int] -> [(Int, Int)]
    getranges = \case
      []         -> []
      fst : rest -> helper fst fst rest
      where
        helper start prev = \case
          []         -> [(start, prev)]
          nxt : tail ->
            if prev + 1 == nxt
            then helper start nxt tail
            else (start, prev) : helper nxt nxt tail

-- | The expression precedence of the empty string in a FLex regex.
precedenceEmpty :: Int
precedenceEmpty = 3

-- | The expression precedence of a single character in a FLex regex.
precedenceOnebyte :: Int
precedenceOnebyte = 3

-- | The expression precedence of a byte set (character class) in a FLex regex.
precedenceByteset :: Int
precedenceByteset = 3

-- | The precedence of a Kleene star expression in a FLex regex.
precedenceStar :: Int
precedenceStar = 2

-- | The precedence of a question mark expression in a FLex regex.
precedenceOptional :: Int
precedenceOptional = 2

-- | The precedence of a plus expression in a FLex regex.
precedencePlus :: Int
precedencePlus = 2

-- | The precedence of a sequence expression in a FLex regex.
precedenceConcat :: Int
precedenceConcat = 1

-- | The precedence of an OR expression in a FLex regex.
precedenceOr :: Int
precedenceOr = 0

-- | Returns the precedence of a 'FlexRegex' node.
flexRegexPrecedence :: FlexRegex -> Int
flexRegexPrecedence = \case
  Empty        -> precedenceEmpty
  Onebyte  _   -> precedenceOnebyte
  Byteset  _   -> precedenceByteset
  Star     _   -> precedenceStar
  Optional _   -> precedenceOptional
  Plus     _   -> precedencePlus
  Concat   _ _ -> precedenceConcat
  Or       _ _ -> precedenceOr

------------------------------------------------------------------------
-- * Conversions.
------------------------------------------------------------------------

-- | Converts a 'BNFC.RegexMinus.SimpleRegex' over @Int8@ into a 'FlexRegex'.
-- Removes minuses from regexes. (0)
--
-- Simplifies the following (where @Phi@ is an empty language):
--
-- > (1)  aa*           -> a+
-- > (2)  r|Phi         -> r
-- > (3)  r|""          -> r?
-- > (4)  tree of Seq   -> list
-- > (5)  a"", ""a      -> a
-- > (6)  aPhi, Phia    -> Phi
-- > (7)  tree of Or    -> set
-- > (8)  [set1]|[set2] -> [set1set2]
-- > (9)  ""*           -> ""
-- > (10) Phi*          -> Phi
fromSimpleRegex :: Minus.SimpleRegex Int8 -> FlexRegex
fromSimpleRegex simplereg = case Minus.removeMinuses simplereg of  -- (0)
  Minus.Term byte     -> Onebyte $ fromIntegral byte
  Minus.Lambda        -> Empty
  Minus.Phi           -> byteset ""
  Minus.Rep r         -> case fromSimpleRegex r of
      Empty            -> Empty  -- (9)
      bs@(Byteset set) -> if null set then bs else Star bs  -- (10)
      other -> Star other
  or@(Minus.Or _ _)   -> let
      regexSet                = makeRegexSetFromOr or  -- (7)
      (bytesets, nonbytesets) = partitionEithers . map
        (\case
          Byteset set -> Left set
          Onebyte b   -> Left (Set.singleton b)
          other       -> Right other) $
        Set.toList regexSet
      onebyteset              = Set.unions bytesets  -- (2) (8)
      unifiedRegexes          =
        if null onebyteset
        then nonbytesets
        else Byteset onebyteset : nonbytesets
    -- (Empty `elem` unifiedRegexes) iff it is an `elem` of regexSet
    in
      if Empty `elem` regexSet
      then Optional . flexOr . delete Empty $ unifiedRegexes  -- (3)
      else flexOr unifiedRegexes
  Minus.Sub _ _       -> error "removeMinuses returned a Sub"
  seq@(Minus.Seq _ _) -> let
      regexList   = makeRegexListFromSeq seq  -- (4)
      listNoEmpty = filter (\case Empty -> False; _ -> True) regexList  -- (5)
      emptyset    = byteset ""
    in
      if emptyset `elem` listNoEmpty then emptyset  -- (6)
      else flexConcat $ foldr foldStar2Plus [] listNoEmpty -- (1)
  where
    makeRegexSetFromOr = \case
      Minus.Or a b -> Set.union (makeRegexSetFromOr a) (makeRegexSetFromOr b)
      reg          -> Set.singleton $ fromSimpleRegex reg
    makeRegexListFromSeq = \case
      Minus.Seq a b -> makeRegexListFromSeq a ++ makeRegexListFromSeq b
      reg           -> [fromSimpleRegex reg]
    foldStar2Plus elem = \case -- case tail of
      []                   -> [elem]
      tail@(Star reg : xs) ->
        if reg == elem
        then Plus elem : xs
        else elem : tail
      tail                 -> elem : tail

-- | Converts a canonical 'BNFC.Abs.Reg' to a 'BNFC.RegexMinus.SimpleRegex' over
-- @Int8@. @char@ (any character, v'BNFC.Abs.RAny') is defined as any valid
-- UTF-8 sequence.
fromBNFCReg :: BNFC.Abs.Reg -> Minus.SimpleRegex Int8
fromBNFCReg = Minus.toSimpleRegex
  (Minus.string . utf8encode . ord)                       -- char2regex
  anyUTF8                                                 -- any
  (Minus.charset [byte_0..byte_9])                        -- digit
  (Minus.charset ([byte_A..byte_Z] ++ [byte_a..byte_z]))  -- letter
  (Minus.charset ([byte_A..byte_Z]))                      -- upper
  (Minus.charset ([byte_a..byte_z]))                      -- lower
  where
    byte_0 = fromIntegral (ord '0') :: Int8
    byte_9 = fromIntegral (ord '9') :: Int8
    byte_a = fromIntegral (ord 'a') :: Int8
    byte_z = fromIntegral (ord 'z') :: Int8
    byte_A = fromIntegral (ord 'A') :: Int8
    byte_Z = fromIntegral (ord 'Z') :: Int8
    byterange :: Int -> Int -> Minus.SimpleRegex Int8
    byterange from to = Minus.charset (map fromIntegral [from..to] :: [Int8])
    -- See @man 7 utf-8@
    utf8Continuation :: Int -> Minus.SimpleRegex Int8
    utf8Continuation n = foldr1 Minus.Seq $ replicate n $ byterange 0x80 0xbf
    anyUTF8 = foldr1 Minus.Or
      [ byterange 0 127
      , byterange 0xc0 0xdf `Minus.Seq` utf8Continuation 1
      , byterange 0xe0 0xef `Minus.Seq` utf8Continuation 2
      , byterange 0xf0 0xf7 `Minus.Seq` utf8Continuation 3
      , byterange 0xf8 0xfb `Minus.Seq` utf8Continuation 4
      , byterange 0xfc 0xfd `Minus.Seq` utf8Continuation 5
      ]
