{-
    BNF Converter: Abstract syntax
    Copyright (C) 2004  Author:  Aarne Ranta

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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

module BNFC.Utils
    ( (+++), (++++), (+-+), (+.+)
    , mkName, mkNames, NameStyle(..)
    , lowerCase, upperCase, mixedCase, camelCase, snakeCase
    , replace, prParenth
    , writeFileRep
    , cstring, cchar
    ) where

import Control.Arrow ((&&&))
import Control.DeepSeq (rnf)

import Data.Char
import Data.List (intercalate)

import System.IO (IOMode(ReadMode),hClose,hGetContents,openFile)
import System.IO.Error (tryIOError)
-- import System.Directory (renameFile, removeFile)

import BNFC.PrettyPrint

infixr 5 +++, ++++, +-+, +.+

-- printing operations

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

-- | Parenthesize a string unless it is empty.
prParenth :: String -> String
prParenth s = if s == "" then "" else "(" ++ s ++ ")"

-- * List utilities

-- | Replace all occurences of a value by another value
replace :: Eq a =>
           a -- ^ Value to replace
        -> a -- ^ Value to replace it with
        -> [a] -> [a]
replace x y xs = [ if z == x then y else z | z <- xs]

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
    notReserved s
      | s `elem` reserved = notReserved (s ++ "_")
      | otherwise = s
    tokens = parseIdent s
    name' = case style of
        LowerCase -> map toLower (concat tokens)
        UpperCase -> map toUpper (concat tokens)
        CamelCase -> concatMap capitalize tokens
        MixedCase -> case concatMap capitalize tokens of
                         "" -> ""
                         c:cs -> toLower c:cs
        SnakeCase -> map toLower (intercalate "_" tokens)
    capitalize [] = []
    capitalize (c:cs) = toUpper c:cs

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
camelCase = text . mkName [] CamelCase

-- | To mixed case.
-- >>> mixedCase "my_ident"
-- myIdent

mixedCase :: String -> Doc
mixedCase = text . mkName [] MixedCase

-- | To snake case.
-- >>> snakeCase "MyIdent"
-- my_ident

snakeCase :: String -> Doc
snakeCase = text . mkName [] SnakeCase

-- ESCAPING

-- | a function that renders a c-like string with escaped characters.
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


-- | A function that renders a c-like character.
--
-- >>> cchar 'x'
-- 'x'
--
-- >>> cchar '\''
-- '\''
cchar :: Char -> Doc
cchar = text . show
