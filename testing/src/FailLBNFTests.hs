-- | Driver for testing BNFC on ill-formed LBNF files.

module FailLBNFTests (all) where

import Prelude hiding (all)
import qualified Data.Text as T
import Shelly
import System.FilePath (dropExtension)
import Test.HUnit (assertBool)
import TestUtils

all :: IO Test
all = do
  stems <- shelly $ do
    map dropExtension <$> findFilesRegex "fail-lbnf" ".*\\.cf"
  return $ makeTestSuite "Testing BNFC on ill-formed LBNF input" $
    map failingTest stems

failingTest :: FilePath -> Test
failingTest stem = makeShellyTest stem $
  errExit False $ do
    -- Run BNFC, but do not produce any output.
    cmd "bnfc" ["--check" , stem <.> "cf"]
    code <- lastExitCode
    err  <- lastStderr
    assertEqual code 1

    -- If .err file exists, compare output against content of .err file.
    let errFile = stem <.> "err"
    errFileExists <- test_f errFile
    when errFileExists $ do
      expectedErr <- liftIO $ {-T.unpack <$>-} readFile errFile
      let canon = unwords . words
      assertEqual (canon expectedErr) (canon $ T.unpack err)
