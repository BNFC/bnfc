{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}  -- ghc 7.10

module BNFC.Utils
    ( ModuleName
    , when, unless, unlessNull, unlessNull'
    , applyWhen, applyUnless
    , for, whenJust
    , caseMaybe, (>.>)
    , curry3, uncurry3
    , singleton, headWithDefault, mapHead, spanEnd
    , duplicatesOn, groupOn, uniqOn
    , hasNumericSuffix
    , (+++), (++++), (+-+), (+.+), parensIf
    , pad, table
    , mkName, mkNames, NameStyle(..)
    , lowerCase, upperCase, mixedCase
    , camelCase, camelCase_
    , snakeCase, snakeCase_
    , replace
    , writeFileRep
    , cstring
    , getZonedTimeTruncatedToSeconds
    , symbolToName
    ) where

import Control.Arrow   ((&&&))
import Control.DeepSeq (rnf)

import Data.Char
import Data.List          (intercalate, transpose)
import Data.List.NonEmpty (pattern (:|))
import Data.Map           (Map)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup     (Semigroup(..))
#endif
import Data.Time

import qualified Data.Foldable      as Fold
import qualified Data.Map           as Map
import qualified Data.List.NonEmpty as List1

import System.IO       (IOMode(ReadMode),hClose,hGetContents,openFile)
import System.IO.Error (tryIOError)

import BNFC.PrettyPrint (Doc, text)

type List1 = List1.NonEmpty

-- | The name of a module, e.g. "Foo.Abs", "Foo.Print" etc.
type ModuleName = String

-- * Control flow.

-- ghc 7.10 misses the instance Monoid a => Monoid (IO a)

#if __GLASGOW_HASKELL__ <= 710
instance {-# OVERLAPPING #-} Semigroup (IO ()) where
  (<>) = (>>)

instance {-# OVERLAPPING #-} Monoid (IO ()) where
  mempty  = return ()
  mappend = (<>)
  mconcat = sequence_
#endif

-- | Generalization of 'Control.Monad.when'.
when :: Monoid m => Bool -> m -> m
when True  m = m
when False _ = mempty

-- | Generalization of 'Control.Monad.unless'.
unless :: Monoid m => Bool -> m -> m
unless False m = m
unless True  _ = mempty

-- | 'when' for the monoid of endofunctions 'a -> a'.
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f = f
applyWhen False _ = id

-- | 'unless' for the monoid of endofunctions 'a -> a'.
applyUnless :: Bool -> (a -> a) -> a -> a
applyUnless False f = f
applyUnless True  _ = id

-- | Invoke continuation for non-empty list.
unlessNull :: Monoid m => [a] -> ([a] -> m) -> m
unlessNull l k = case l of
  [] -> mempty
  as -> k as

-- | Invoke continuation for non-empty list.
unlessNull' :: Monoid m => [a] -> (a -> [a] -> m) -> m
unlessNull' l k = case l of
  []     -> mempty
  (a:as) -> k a as

-- * Flipped versions of standard functions.

infixr 8 >.>

-- | Diagrammatic composition.
(>.>) :: (a -> b) -> (b -> c) -> a -> c
g >.> f = f . g

-- | Non-monadic 'forM'.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Generalization of 'forM' to 'Monoid'.
whenJust :: Monoid m => Maybe a -> (a -> m) -> m
whenJust = flip foldMap

-- | Rotation of 'maybe'.
caseMaybe :: Maybe a -> b -> (a -> b) -> b
caseMaybe ma b f = maybe b f ma

-- * Tuple utilities.

-- From https://hackage.haskell.org/package/extra-1.6.18/docs/Data-Tuple-Extra.html

-- | Converts an uncurried function to a curried function.
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

-- * String operations for printing.

infixr 5 +++, ++++, +-+, +.+

-- | Concatenate strings by a space.
(+++) :: String -> String -> String
a +++ b   = a ++ " "    ++ b

-- | Concatenate strings by a newline.
(++++) :: String -> String -> String
a ++++ b  = a ++ "\n"   ++ b

-- | Concatenate strings by an underscore.
(+-+) :: String -> String -> String
a +-+ b   = a ++ "_"    ++ b

-- | Concatenate strings by a dot.
(+.+) :: String -> String -> String
a +.+ b   = a ++ "."    ++ b

-- | Wrap in parentheses if condition holds.
parensIf :: Bool -> String -> String
parensIf = \case
  True  -> ("(" ++) . (++ ")")
  False -> id

-- | Pad a string on the right by spaces to reach the desired length.
pad :: Int -> String -> String
pad n s = s ++ drop (length s) (replicate n ' ')

-- | Make a list of rows with left-aligned columns from a matrix.
table :: String -> [[String]] -> [String]
table sep m = map (intercalate sep . zipWith pad widths) m
  where
  -- Column widths.
  widths :: [Int]
  widths = columns maximum $ map (map length) m
  -- Aggregate columns (works even for a ragged matrix with rows of different length).
  columns :: ([a] -> b) -> [[a]] -> [b]
  columns f = map f . transpose

-- * List utilities

-- | Give a telling name to the electric monkey.
singleton :: a -> [a]
singleton = (:[])

-- | Get the first element of a list, fallback for empty list.
headWithDefault :: a -> [a] -> a
headWithDefault a []    = a
headWithDefault _ (a:_) = a

-- | Apply a function to the head of a list.
mapHead :: (a -> a) -> [a] -> [a]
mapHead f = \case
 []   -> []
 a:as -> f a : as

-- | @spanEnd p l == reverse (span p (reverse l))@.
--
-- Invariant: @l == front ++ end where (end, front) = spanEnd p l@
--
-- (From package ghc, module Util.)
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p l = go l [] [] l
  where
  go yes _        rev_no [] = (yes, reverse rev_no)
  go yes rev_yes  rev_no (x:xs)
    | p x       = go yes (x : rev_yes) rev_no                  xs
    | otherwise = go xs  []            (x : rev_yes ++ rev_no) xs


-- | Replace all occurences of a value by another value
replace :: Eq a =>
           a -- ^ Value to replace
        -> a -- ^ Value to replace it with
        -> [a] -> [a]
replace x y xs = [ if z == x then y else z | z <- xs]

-- | Returns lists of elements whose normal form appears more than once.
--
-- >>> duplicatesOn id  [5,1,2,5,1]
-- [1 :| [1],5 :| [5]]
-- >>> duplicatesOn abs [5,-5,1]
-- [5 :| [-5]]
duplicatesOn :: (Foldable t, Ord b) => (a -> b) -> t a -> [List1 a]
duplicatesOn nf
    -- Keep groups of size >= 2.
  = filter ((2 <=) . List1.length)
    -- Turn into a list of listss: elements grouped by their normal form.
  . Map.elems
    -- Partition elements by their normal form.
  . Fold.foldr (\ a -> Map.insertWith (<>) (nf a) (a :| [])) Map.empty

-- | Group consecutive elements that have the same normalform.
groupOn :: Eq b => (a -> b) -> [a] -> [List1 a]
groupOn nf = loop
  where
  loop = \case
    []   -> []
    a:xs | let (as, rest) = span ((nf a ==) . nf) xs
         -> (a :| as) : loop rest

-- | Keep only the first of consecutive elements that have the same normalform.
uniqOn :: Eq b => (a -> b) -> [a] -> [a]
uniqOn nf = map List1.head . groupOn nf

-- | Get a numeric suffix if it exists.
--
-- >>> hasNumericSuffix "hello world"
-- Nothing
-- >>> hasNumericSuffix "a1b2"
-- Just ("a1b",2)
-- >>> hasNumericSuffix "1234"
-- Just ("",1234)

hasNumericSuffix :: String -> Maybe (String, Integer)
hasNumericSuffix s = case spanEnd isDigit s of
  ([], _) -> Nothing
  (num, front) -> Just (front, read num)

-- * Time utilities

-- | Cut away fractions of a second in time.

truncateZonedTimeToSeconds :: ZonedTime -> ZonedTime
truncateZonedTimeToSeconds (ZonedTime (LocalTime day (TimeOfDay h m s)) zone) =
  ZonedTime (LocalTime day (TimeOfDay h m $ fromIntegral sec)) zone
  where
  sec :: Int
  sec = truncate s

getZonedTimeTruncatedToSeconds :: IO ZonedTime
getZonedTimeTruncatedToSeconds = truncateZonedTimeToSeconds <$> getZonedTime

-- * File utilities

-- | Write a file, after making a backup of an existing file with the same name.
-- If an old version of the file exist and the new version is the same,
-- keep the old file and don't create a .bak file.
-- / New version by TH, 2010-09-23

writeFileRep :: FilePath -> String -> IO ()
writeFileRep path s =
    either newFile updateFile =<< tryIOError (readFile' path)
  where
    -- Case: file does not exist yet.
    newFile _ = do
      putStrLn $ "writing new file " ++ path
      writeFile path s

    -- Case: file exists with content @old@.
    updateFile old = do
      -- Write new content.
      writeFile path s
      if s == old  -- test is O(1) space, O(n) time
         then do
           putStrLn $ "refreshing unchanged file " ++ path
         else do
           let bak = path ++ ".bak"
           putStrLn $ "writing file " ++ path ++ " (saving old file as " ++ bak ++ ")"
           writeFile bak old

    -- Force reading of contents of files to achieve compatibility with
    -- Windows IO handling, as combining lazy IO with `readFile` and
    -- 2x `renameFile` on the open `path` file complains with:
    --
    -- "bnfc.exe: Makefile: MoveFileEx "Makefile" "Makefile.bak": permission
    -- denied (The process cannot access the file because it is being used
    -- by another process.)"
    readFile' :: FilePath -> IO String
    readFile' path' = do
      inFile   <- openFile path' ReadMode
      contents <- hGetContents inFile
      rnf contents `seq` hClose inFile
      return contents

-- *** Naming ***
-- Because naming is hard (http://blog.codinghorror.com/i-shall-call-it-somethingmanager/)

-- | Different case style
data NameStyle
  = LowerCase  -- ^ e.g. @lowercase@
  | UpperCase  -- ^ e.g. @UPPERCASE@
  | SnakeCase  -- ^ e.g. @snake_case@
  | CamelCase  -- ^ e.g. @CamelCase@
  | MixedCase  -- ^ e.g. @mixedCase@
  | OrigCase   -- ^ Keep original capitalization and form.
  deriving (Show, Eq)

-- | Generate a name in the given case style taking into account the reserved
-- word of the language. Note that despite the fact that those name are mainly
-- to be used in code rendering (type Doc), we return a String here to allow
-- further manipulation of the name (like disambiguation) which is not possible
-- in the Doc type.
--
-- Examples:
--
-- >>> mkName [] LowerCase "FooBAR"
-- "foobar"
--
-- >>> mkName [] UpperCase "FooBAR"
-- "FOOBAR"
--
-- >>> mkName [] SnakeCase "FooBAR"
-- "foo_bar"
--
-- >>> mkName [] CamelCase "FooBAR"
-- "FooBAR"
--
-- >>> mkName [] CamelCase "Foo_bar"
-- "FooBar"
--
-- >>> mkName [] MixedCase "FooBAR"
-- "fooBAR"
--
-- >>> mkName ["foobar"] LowerCase "FooBAR"
-- "foobar_"
--
-- >>> mkName ["foobar", "foobar_"] LowerCase "FooBAR"
-- "foobar__"

mkName :: [String] -> NameStyle -> String -> String
mkName reserved style s = notReserved name'
  where
    notReserved name
      | name `elem` reserved = notReserved (name ++ "_")
      | otherwise = name
    tokens = parseIdent s
    name' = case style of
        LowerCase -> map toLower $ concat tokens
        UpperCase -> map toUpper $ concat tokens
        CamelCase -> concatMap capitalize tokens
        MixedCase -> mapHead toLower $ concatMap capitalize tokens
        SnakeCase -> map toLower $ intercalate "_" tokens
        OrigCase  -> s
    capitalize = mapHead toUpper

-- | Same as above but accept a list as argument and make sure that the
-- names generated are uniques.
--
-- >>> mkNames ["c"] LowerCase ["A", "b_", "a_", "c"]
-- ["a1","b","a2","c_"]

mkNames :: [String] -> NameStyle -> [String] -> [String]
mkNames reserved style = disambiguateNames . map (mkName reserved style)

-- | This one takes a list of names and makes sure each is unique, appending
-- numerical suffix if needed.
--
-- >>> disambiguateNames ["a", "b", "a", "c"]
-- ["a1","b","a2","c"]

disambiguateNames :: [String] -> [String]
disambiguateNames = disamb []
  where
    disamb ns1 (n:ns2)
      | n `elem` (ns1 ++ ns2) = let i = length (filter (==n) ns1) + 1
                                in (n ++ show i) : disamb (n:ns1) ns2
      | otherwise = n : disamb (n:ns1) ns2
    disamb _ [] = []

-- | Heuristic to "parse" an identifier into separate components.
--
-- >>> parseIdent "abc"
-- ["abc"]
--
-- >>> parseIdent "Abc"
-- ["Abc"]
--
-- >>> parseIdent "WhySoSerious"
-- ["Why","So","Serious"]
--
-- >>> parseIdent "why_so_serious"
-- ["why","so","serious"]
--
-- >>> parseIdent "why-so-serious"
-- ["why","so","serious"]
--
-- Some corner cases:
--
-- >>> parseIdent "LBNFParser"
-- ["LBNF","Parser"]
--
-- >>> parseIdent "aLBNFParser"
-- ["a","LBNF","Parser"]
--
-- >>> parseIdent "ILoveNY"
-- ["I","Love","NY"]

parseIdent :: String -> [String]
parseIdent = p [] . map (classify &&& id)
  where
    classify :: Char -> CharClass
    classify c
        | isUpper c = U
        | isLower c = L
        | otherwise = O

    p :: String -> [(CharClass,Char)] -> [String]
    -- Done:
    p acc []                       = emit acc []

    -- Continue if consecutive characters have same case.
    p acc ((L,c) : cs@((L,_) : _)) = p (c:acc) cs
    p acc ((U,c) : cs@((U,_) : _)) = p (c:acc) cs

    -- Break if consecutive characters have different case.
    p acc ((U,c) : cs@((L,_) : _)) = emit acc     $ p [c] cs
    p acc ((L,c) : cs@((U,_) : _)) = emit (c:acc) $ p [] cs

    -- Discard "other" characters, and break to next component.
    p acc ((U,c) :     (O,_) : cs) = emit (c:acc) $ p [] cs
    p acc ((L,c) :     (O,_) : cs) = emit (c:acc) $ p [] cs
    p acc ((O,_) : cs)             = emit acc     $ p [] cs

    p acc [(_,c)]                  = p (c:acc) []

    emit :: String -> [String] -> [String]
    emit []  rest = rest
    emit acc rest = reverse acc : rest

data CharClass = U | L | O

-- | Ident to lower case.
-- >>> lowerCase "MyIdent"
-- myident

lowerCase :: String -> Doc
lowerCase = text . mkName [] LowerCase

-- | Ident to upper case.
-- >>> upperCase "MyIdent"
-- MYIDENT

upperCase :: String -> Doc
upperCase = text . mkName [] UpperCase

-- | Ident to camel case.
-- >>> camelCase "my_ident"
-- MyIdent

camelCase :: String -> Doc
camelCase = text . camelCase_

camelCase_ :: String -> String
camelCase_ = mkName [] CamelCase

-- | To mixed case.
-- >>> mixedCase "my_ident"
-- myIdent

mixedCase :: String -> Doc
mixedCase = text . mkName [] MixedCase

-- | To snake case.
-- >>> snakeCase "MyIdent"
-- my_ident

snakeCase :: String -> Doc
snakeCase = text . snakeCase_

snakeCase_ :: String -> String
snakeCase_ = mkName [] SnakeCase

-- ESCAPING

-- | A function that renders a c-like string with escaped characters.
-- Note that although it's called cstring, this can be used with most (all)
-- backend as they seem to mostly share escaping conventions.
-- The c in the name is barely an homage for C being the oldest language in
-- the lot.
--
-- >>> cstring "foobar"
-- "foobar"
--
-- >>> cstring "foobar\""
-- "foobar\""
cstring :: String -> Doc
cstring = text . show

-- * Symbols

-- | Print a symbol as typical token name, like "(" as "LPAREN".

symbolToName :: String -> Maybe String
symbolToName = (`Map.lookup` symbolTokenMap)

-- | Map from symbol to token name.

symbolTokenMap :: Map String String
symbolTokenMap = Map.fromList symbolTokenList

-- | Association list from symbol to token name.

symbolTokenList :: [(String, String)]
symbolTokenList =
  [ ("{"  , "LBRACE")
  , ("}"  , "RBRACE")
  , ("("  , "LPAREN")
  , (")"  , "RPAREN")
  , ("["  , "LBRACK")
  , ("]"  , "RBRACK")
  , ("[]" , "EMPTYBRACK")
  , ("."  , "DOT")
  , (".." , "DDOT")
  , ("...", "ELLIPSIS")
  , (","  , "COMMA")
  , (",," , "DCOMMA")
  , (";"  , "SEMI")
  , (";;" , "DSEMI")
  , (":"  , "COLON")
  , ("::" , "DCOLON")
  , (":=" , "COLONEQ")
  , ("::=", "DCOLONEQ")
  , (":-" , "COLONMINUS")
  , ("::-", "DCOLONMINUS")
  , (":--", "COLONDMINUS")
  , ("+"  , "PLUS")
  , ("++" , "DPLUS")
  , ("+=" , "PLUSEQ")
  , ("+-" , "PLUSMINUS")
  , ("-"  , "MINUS")
  , ("--" , "DMINUS")
  , ("-=" , "MINUSEQ")
  , ("-+" , "MINUSPLUS")
  , ("-*" , "MINUSSTAR")
  , ("*"  , "STAR")
  , ("**" , "DSTAR")
  , ("*=" , "STAREQ")
  , ("*-" , "STARMINUS")
  , ("/"  , "SLASH")
  , ("//" , "DSLASH")
  , ("/=" , "SLASHEQ")
  , ("\\" , "BACKSLASH")
  , ("\\\\","DBACKSLASH")
  , ("/\\", "WEDGE")
  , ("\\/", "VEE")
  , ("&"  , "AMP")
  , ("&&" , "DAMP")
  , ("&=" , "AMPEQ")
  , ("|"  , "BAR")
  , ("||" , "DBAR")
  , ("|=" , "BAREQ")
  , ("<"  , "LT")
  , ("<<" , "DLT")
  , ("<<<", "TLT")
  , ("<=" , "LTEQ")
  , ("<<=", "DLTEQ")
  , ("<<<=","TLTEQ")
  , (">"  , "GT")
  , (">>" , "DGT")
  , (">>>", "TGT")
  , (">=" , "GTEQ")
  , (">>=", "DGTEQ")
  , (">>>=","TGTEQ")
  , ("<>" , "LTGT")
  , ("="  , "EQ")
  , ("==" , "DEQ")
  , ("_"  , "UNDERSCORE")
  , ("!"  , "BANG")
  , ("!=" , "BANGEQ")
  , ("?"  , "QUESTION")
  , ("?=" , "QUESTIONEQ")
  , ("#"  , "HASH")
  , ("##" , "DHASH")
  , ("###", "THASH")
  , ("@"  , "AT")
  , ("@@" , "DAT")
  , ("@=" , "ATEQ")
  , ("$"  , "DOLLAR")
  , ("$$" , "DDOLLAR")
  , ("%"  , "PERCENT")
  , ("%%" , "DPERCENT")
  , ("%=" , "PERCENTEQ")
  , ("^"  , "CARET")
  , ("^^" , "DCARET")
  , ("^=" , "CARETEQ")
  , ("~"  , "TILDE")
  , ("~~" , "DTILDE")
  , ("~=" , "TILDEEQ")
  , ("'"  , "APOSTROPHE")
  , ("''" , "DAPOSTROPHE")
  , ("'''", "TAPOSTROPHE")
  , ("<-" , "LARROW")
  , ("->" , "RARROW")
  , ("<=" , "LDARROW")
  , ("=>" , "RDARROW")
  , ("|->", "MAPSTO")
  ]
