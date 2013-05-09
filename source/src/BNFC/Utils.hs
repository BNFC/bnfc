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

module BNFC.Utils where

import Control.Monad (unless)
import System.IO.Error (tryIOError)
import System.Directory (createDirectory, doesDirectoryExist, renameFile,
                         removeFile, doesFileExist)

import System.FilePath (pathSeparator)

infixr 5 +++
infixr 5 ++++
infixr 5 +++++
infixr 2 |||
infixr 5 ...
infixr 3 ***


-- printing operations

a +++ b   = a ++ " "    ++ b
a ++++ b  = a ++ "\n"   ++ b
a +++++ b = a ++ "\n\n" ++ b

prParenth s = if s == "" then "" else "(" ++ s ++ ")"


-- parser combinators a` la Wadler and Hutton

type Parser a b = [a] -> [(b,[a])]

(...) :: Parser a b -> Parser a c -> Parser a (b,c)
(p ... q) s = [((x,y),r) | (x,t) <- p s, (y,r) <- q t]

(|||) :: Parser a b -> Parser a b -> Parser a b
(p ||| q) s = p s ++ q s

lit :: (Eq a) => a -> Parser a a
lit x (c:cs) = [(x,cs) | x == c]
lit _ _ = []

(***) :: Parser a b -> (b -> c) -> Parser a c
(p *** f) s = [(f x,r) | (x,r) <- p s]

succeed :: b -> Parser a b
succeed v s = [(v,s)]

fails :: Parser a b
fails s = []

-- to get parse results

parseResults :: Parser a b -> [a] -> [b]
parseResults p s = [x | (x,r) <- p s, null r]


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
writeFileRep :: FilePath -> String -> IO ()
writeFileRep = writeFileRep2

-- peteg: FIXME this is racey.
-- want to be a bit smarter about whether we actually generate the file
-- or save it... e.g. ErrM.hs need not be regenerated if it exists.
writeFileRep1 f s =
    do exists <- doesFileExist f
       backedUp <- if exists
		     then do let fbak = f ++ ".bak"
		             renameFile f fbak
			     return $ " (saving old file as " ++ fbak ++ ")"
		     else return ""
       putStrLn $ "writing file " ++ f ++ backedUp
       writeFile f s

-- New version by TH, 2010-09-23
-- If an old version of the file exist and the new version is the same,
-- keep the old file and don't create a .bak file.
writeFileRep2 :: FilePath -> String -> IO ()
writeFileRep2 path s =
    either newFile updateFile =<< tryIOError (readFile path)
  where
    newFile _ =
        do putStrLn $ "writing new file "++path
           writeFile path s
    updateFile old =
        do let tmp=path++".tmp"
           writeFile tmp s
           new <- readFile tmp
           if new==old  -- test is O(1) space, O(n) time
              then do putStrLn $ "no change to file "++path
                      removeFile tmp
              else do let bak=path++".bak"
                      putStrLn $ "writing file "++path
                                   ++" (saving old file as "++bak++")"
                      renameFile path bak
                      renameFile tmp path
