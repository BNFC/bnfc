{-
    BNF Converter: Abstract syntax
    Copyright (C) 2004  Author: Markus Forsberg, Aarne Ranta

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


module BNFC.MultiView where

import Data.Maybe

import ParBNF
import PrintBNF
import AbsBNF

preprocessMCF :: FilePath -> IO ([FilePath],String)
preprocessMCF f = do
  s  <- readFile f
  gr <- case pLGrammar $ myLexer s of
    Right g  -> return g
    Left s -> error s
  let name = takeWhile (/='.') f
  let grs = extract name gr
  let entryp = entrypoint gr
  mapM_ writeCF grs
  return $ (map fst grs,entryp)

extract :: String -> LGrammar -> [(FilePath, Grammar)]
extract name (LGr ldefs) =
  [(file lang,Grammar $ mapMaybe (getDef lang) ldefs) |
      lang <- views]
 where
   views = [lang | LDefView langs <- ldefs, Identifier lang <- langs]
   getDef lang ldef = case ldef of
     DefAll d -> Just d
     DefSome ids d | elem (Identifier lang) ids -> Just d
     _ -> Nothing
   file lang = name ++ "_" ++ lang ++ ".cf"

--- the entrypoint is the same for all languages - could be different

entrypoint :: LGrammar -> String
entrypoint (LGr rs0) = head $
  [c | Entryp (Identifier c:_) <- rs] ++
  [c | Rule _ (IdCat (Identifier c)) _ <- rs]
 where
   rs = mapMaybe getR rs0
   getR d = case d of
     DefAll d -> Just d
     DefSome _ d -> Just d
     _LDefView -> Nothing

writeCF :: (FilePath, Grammar) -> IO ()
writeCF (file,gr) = do
  writeFile file $ printTree gr
  putStrLn $ "wrote file " ++ file

---- These are Haskell specific;
---- should be generalized by inspecting the options xx

mkTestMulti :: String -> [String] -> FilePath -> [FilePath] -> IO ()
mkTestMulti cat xx file files = do
  let abs  = takeWhile (/='.') file
  let cncs = map (takeWhile (/='.')) files
  let content = testfile cat xx abs cncs
  writeFile ("TestTrans" ++ abs ++ ".hs") content

mkMakefileMulti :: [String] -> FilePath -> [FilePath] -> IO ()
mkMakefileMulti xx file files = do
  let abs  = takeWhile (/='.') file
  let cncs = map (takeWhile (/='.')) files
  let content = makefile xx abs cncs
  writeFile "Makefile" content

makefile _ abs cncs = unlines $
  "all:" :
  ["\tmake -f Makefile_" ++ cnc | cnc <- cncs] ++
  ["\tghc --make -o TestTrans" ++ abs ++ " TestTrans" ++ abs,
   ""
  ]

testfile cat _ abs cncs = unlines $
  ["module Main where"] ++
  ["import qualified Lex" ++ cnc | cnc <- cncs] ++
  ["import qualified Par" ++ cnc | cnc <- cncs] ++
  ["import qualified Print" ++ cnc | cnc <- cncs] ++
  ["import Abs" ++ abs,
   "import System.Environment (getArgs)",
   "",
   "main :: IO ()",
   "main = do",
   "  i:o:f:_ <- getArgs",
   "  s <- readFile f",
   "  case parse i s of",
   "    Right t -> putStrLn $ prin o t",
   "    Left  s -> error s",
   "",
   "parse i = case i of"
  ] ++
  [
   "  " ++ sho cnc ++ " -> Par" ++ cnc ++ ".p" ++ cat ++
        " . Par" ++ cnc ++ ".myLexer" | cnc <- cncs
  ] ++
  [
   "",
   "prin o = case o of"
  ] ++
  [
   "  " ++ sho cnc ++ " -> Print" ++ cnc ++
                                         ".printTree" | cnc <- cncs
  ]
 where
   sho = show . tail . dropWhile (/='_')
