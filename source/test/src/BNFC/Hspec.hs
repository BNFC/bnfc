-- | Custom hspec expectation
module BNFC.Hspec where

import Text.Printf

import BNFC.Backend.Base

import Test.Hspec
import Test.HUnit ((@?))


-- | expectation that a backend generate a particular file
shouldGenerate :: Backend -> String -> Expectation
backend `shouldGenerate` file = do
  files <- execBackend backend
  let filenames = map fst files
  file `elem` filenames
    @? printf "file %s not found in %s" file (show filenames)

