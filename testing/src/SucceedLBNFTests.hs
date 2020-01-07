-- | Driver for testing BNFC on ill-formed LBNF files.

module SucceedLBNFTests (all) where

import Prelude hiding (all)
import qualified Data.Text as T
import Shelly
import System.FilePath (dropExtension)
-- import Test.HUnit (assertBool)
import TestUtils

all :: IO Test
all = do
  stems <- shelly $ do
    map dropExtension <$> findFilesRegex "succeed-lbnf" ".*\\.cf"
  return $ makeTestSuite "Testing BNFC on well-formed LBNF input" $
    map succeedingTest stems

succeedingTest :: FilePath -> Test
succeedingTest stem = makeShellyTest stem $ do

  -- Run BNFC, but do not invoke any backend.
  out <- cmd "bnfc" ["--check" , stem <.> "cf"]

  -- If .out file exists, compare output against content of .out file.
  let outFile = stem <.> "out"
  outFileExists <- test_f outFile
  when outFileExists $ do
    expectedOut <- liftIO $ {-T.unpack <$>-} readFile outFile
    let canon = unwords . words
    assertEqual (canon expectedOut) (canon $ T.unpack out)
