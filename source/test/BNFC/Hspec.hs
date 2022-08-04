-- | Custom hspec expectations.

module BNFC.Hspec where

import Text.Printf

import BNFC.Backend.Base

import Test.Hspec
import Test.HUnit ((@?))


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

shouldNotGenerate
  :: Backend   -- ^ Backend to run.
  -> String    -- ^ Name of file that should be created during that run.
  -> Expectation
backend `shouldNotGenerate` file = do
  files <- execBackend backend
  let filenames = map fileName files
  file `notElem` filenames
    @? printf "unexpected file %s found in %s" file (show filenames)
