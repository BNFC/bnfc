-- | Custom hspec expectations.

module BNFC.Hspec where

import Text.Printf
import Data.Char(isSpace)

import BNFC.Backend.Base

import Test.Hspec
import Test.HUnit ((@?), (@=?), assertFailure)


-- | Expectation that a backend generates a particular file.

shouldGenerate
  :: Backend   -- ^ Backend to run.
  -> String    -- ^ Name of file that should be created during that run.
  -> Expectation
backend `shouldGenerate` file = do
  files <- execBackend backend
  let filenames = map fileName files
  file `elem` filenames
    @? printf "file %s not found in %s" file (show filenames)

shouldGenerateText
  :: Backend           -- ^ Backend to run.
  -> (String, String)  -- ^ Expected name of file and its expected contents.
  -> Expectation
backend `shouldGenerateText` (file, expected) = do
  backendFiles <- execBackend backend
  let files = map (\x -> (fileName x, fileContent x)) backendFiles
  let canon = dropWhile isSpace
  case lookup file files of
    Nothing -> assertFailure $ printf "file %s not found in %s" file (show (map fst files))
    Just content -> canon expected @=? canon content

