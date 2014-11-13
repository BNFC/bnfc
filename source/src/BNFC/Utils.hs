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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module BNFC.Utils
    ( (+++), (++++), (+++++)
    , (<=>)
    , lowerCase, upperCase, camelCase, mixedCase, snakeCase
    , prParenth, replace, split, splitAll, prepareDir
    , writeFileRep
    , vsep
    ) where

import Control.Arrow ((&&&))
import Control.DeepSeq (rnf)
import Control.Monad (unless)
import Data.Char
import Data.List (intercalate)
import System.IO (IOMode(ReadMode),hClose,hGetContents,openFile)
import System.IO.Error (tryIOError)
import System.Directory (createDirectory, doesDirectoryExist, renameFile,
                         removeFile)
import System.FilePath (pathSeparator)
import Text.PrettyPrint

infixr 5 +++
infixr 5 ++++
infixr 5 +++++

-- printing operations

(+++), (++++), (+++++) :: String -> String -> String
a +++ b   = a ++ " "    ++ b
a ++++ b  = a ++ "\n"   ++ b
a +++++ b = a ++ "\n\n" ++ b

prParenth :: String -> String
prParenth s = if s == "" then "" else "(" ++ s ++ ")"

-- * List utilities

-- | Replace all occurences of a value by another value
replace :: Eq a =>
	   a -- ^ Value to replace
	-> a -- ^ Value to replace it with
	-> [a] -> [a]
replace x y xs = [ if z == x then y else z | z <- xs]

-- | Split a list on the first occurence of a value.
--   Does not include the value that was split on in either
--   of the returned lists.
split :: Eq a => a -> [a] -> ([a],[a])
split x xs = let (ys, zs) = break (==x) xs
		 in (ys, drop 1 zs)

-- | Split a list on every occurence of a value.
--   If the value does not occur in the list,
--   the result is the singleton list containing the input list.
--   Thus the returned list is never the empty list.
splitAll :: Eq a => a -> [a] -> [[a]]
splitAll _ [] = [[]]
splitAll x xs = let (ys, zs) = break (==x) xs
		    in ys : case zs of
				    [] -> []
				    _:zs' -> splitAll x zs'

-- * File utilities

-- | Ensure that a directory exists.
prepareDir :: FilePath -> IO ()
prepareDir = mapM_ createDirectoryIfNotExists . pathInits

-- | Ensure that a directory exists. All parent directories
--   must already exist.
createDirectoryIfNotExists :: FilePath -> IO ()
createDirectoryIfNotExists d = do
      exists <- doesDirectoryExist d
      unless exists (createDirectory d)

-- | Like the prelude function 'inits' but for path names.
--   For example:
-- > pathInits "foo/bar" = ["foo","foo/bar"]
-- > pathInits "foo/bar/baz.hs" = ["foo","foo/bar","foo/bar/baz.hs"]
pathInits :: String -> [String]
pathInits "" = []
pathInits xs = let (ys,zs) = split pathSeparator xs
		   in ys : map ((ys ++ [pathSeparator]) ++) (pathInits zs)

-- | Like basename(1), remove all leading directories from a path name.
-- basename :: String -> String
-- basename = last . splitAll pathSeparator


-- | Write a file, after making a backup of an existing file with the same name.
-- If an old version of the file exist and the new version is the same,
-- keep the old file and don't create a .bak file.
-- / New version by TH, 2010-09-23
writeFileRep :: FilePath -> String -> IO ()
writeFileRep path s =
    either newFile updateFile =<< tryIOError (readFile' path)
  where
    newFile _ =
        do putStrLn $ "writing new file "++path
           writeFile path s
    updateFile old =
        do let tmp=path++".tmp"
           writeFile tmp s
           new <- readFile' tmp
           if new==old  -- test is O(1) space, O(n) time
              then do putStrLn $ "no change to file "++path
                      removeFile tmp
              else do let bak=path++".bak"
                      putStrLn $ "writing file "++path
                                   ++" (saving old file as "++bak++")"
                      renameFile path bak
                      renameFile tmp path

    -- force reading of contents of files to achieve compatibility with
    -- Windows IO handling as combining lazy IO with `readFile` and
    -- 2x `renameFile` on the open `path` file complains with
    -- "bnfc.exe: Makefile: MoveFileEx "Makefile" "Makefile.bak": permission
    -- denied (The process cannot access the file because it is being used
    -- by another process.)"
    readFile' :: FilePath -> IO String
    readFile' path' =
        do inFile <- openFile path' ReadMode
           contents <- hGetContents inFile
           rnf contents `seq` hClose inFile
	   return contents

-- | List version of prettyPrint $+$
-- >>> vsep [text "abc", nest 4 (text "def")]
-- abc
--     def
vsep :: [Doc] -> Doc
vsep = foldl ($+$) empty

-- | Pretty print separator with a dot
-- >>> "abc" <.> "py"
-- abc.py
(<.>) :: Doc -> Doc -> Doc
a <.> b = a <> "." <> b

-- | Pretty print separator with = (for assignments...)
-- >>> "a" <=> "123"
-- a = 123
(<=>) :: Doc -> Doc -> Doc
a <=> b = a <+> "=" <+> b


-- | Heuristic to "parse" an identifier and separating componennts
-- >>> parseIdent "abc"
-- ["abc"]
-- >>> parseIdent "Abc"
-- ["abc"]
-- >>> parseIdent "WhySoSerious"
-- ["why","so","serious"]
-- >>> parseIdent "why_so_serious"
-- ["why","so","serious"]
-- >>> parseIdent "why-so-serious"
-- ["why","so","serious"]
--
-- Some corner cases
-- >>> parseIdent "LBNFParser"
-- ["lbnf","parser"]
-- >>> parseIdent "ILoveNY"
-- ["i","love","ny"]
parseIdent :: String -> [String]
parseIdent = p [] . map (classify &&& toLower)
  where
    classify c
        | isUpper c = U
        | isLower c = L
        | otherwise = O
    p [] [] = []
    p acc [] = reverse acc: p [] []
    p [] ((L,c):cs) = p [c] cs
    p [] ((U,c):cs) = p [c] cs
    p [] ((O,_):cs) = p [] cs
    p acc ((L,c1):cs@((L,_):_)) = p (c1:acc) cs
    p acc ((U,c1):cs@((L,_):_)) = reverse acc:p [c1] cs
    p acc ((U,c1):cs@((U,_):_)) = p (c1:acc) cs
    p acc ((L,c1):cs@((U,_):_)) = reverse (c1:acc) : p [] cs
    p acc ((U,c1):(O,_):cs) = reverse (c1:acc) : p [] cs
    p acc ((L,c1):(O,_):cs) = reverse (c1:acc) : p [] cs
    p acc ((O,_):cs) = reverse acc : p [] cs
    p acc [(_,c)] = p (c:acc) []

-- | Ident to lower case
-- >>> lowerCase "MyIdent"
-- myident
lowerCase :: String -> Doc
lowerCase = text . concat . parseIdent
-- | Ident to upper case
-- >>> upperCase "MyIdent"
-- MYIDENT
upperCase :: String -> Doc
upperCase = text . map toUpper . concat . parseIdent
-- | Ident to camel case
-- >>> camelCase "my_IDENT"
-- MyIdent
camelCase :: String -> Doc
camelCase = text . concatMap capitalize . parseIdent
  where capitalize [] = []
        capitalize (c:cs) = toUpper c:map toLower cs
-- | To mixed case
-- >>> mixedCase "MY_IDENT"
-- myIdent
mixedCase :: String -> Doc
mixedCase s = case render (camelCase s) of
    []   -> empty
    c:cs -> text (toLower c:cs)
-- | To snake case
-- >>> snakeCase "MyIdent"
-- my_ident
snakeCase :: String -> Doc
snakeCase = text . intercalate "_" . parseIdent


data CharClass = U | L | O
