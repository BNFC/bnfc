{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module SystemTesting where

import Test.HUnit (assertBool, (@=?), Assertion, assertEqual)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Filesystem.Path (basename, filename)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Text.Printf (printf)
import Shelly
import Prelude hiding (FilePath)
import Data.Text.Lazy (Text)
import Control.Exception (assert)
import Paths_BNFC

default (Text)

-- | we define the test cases: a cf grammar, and an input file
type TestBundle = (FilePath, FilePath)

testName :: TestBundle -> String
testName = encodeString . basename . fst

testCases :: IO [(FilePath, FilePath)]
testCases = do
  dataPath <- shelly $ absPath ("test" </> "data")
  return $ map (\(f1,f2) -> (dataPath </> f1, dataPath </> f2))
    [ -- ( "alfa/alfa.cf",       "alfa/test.alfa" ) Broken
      ( "c/c.cf",       "c/test.c" )
    , ( "cpp/cpp.cf",   "cpp/test.cpp" )
    , ( "gf/gf.cf",     "gf/test.gf" )
    , ( "lbnf/lbnf.cf", "lbnf/test.lbnf" )
    , ( "ocl/ocl.cf",   "ocl/test.ocl" ) ]

systemTestMain :: Backend -> IO ()
systemTestMain backend = testFactory backend >>= defaultMain

-- | Build The Test Suite
testFactory :: Backend -> IO [Test]
testFactory backend = do
  -- first we have to find where bnfc is. To do that,
  -- we use the getBinDir exposed by cabal and turn the returned
  -- string in a FilePath. Then we concatenate “bnfc” to this
  -- path. This might be a problem on windows though...
  --   bin <- getBinDir >>= return . decodeString
  --   let bnfc = cmd ( bin</>"bnfc")
  -- it looks like cabal doesn't set the binbir variable properly
  -- when running tests. We need to use hard coded paths instead.
  bnfcPath <- shelly $ absPath ( "dist"</>"build"</>"bnfc"</>"bnfc")
  cases <- testCases
  -- Then we build a list of test groups (one for each backend)
  -- using the List monad
  return $ do
    testC <- cases
    let name = testName testC
    let (cf,test) = testC
    return $ testCase name (backend bnfcPath cf test)

-- | A backend test the given test file (source code corresponding to the
-- grammar) given a grammar and a binary of bnfc.
-- All path can be assumed to be absolute.
type Backend = FilePath  -- ^ Path to the bnfc binary under test
             -> FilePath  -- ^ Path to the cf grammar
             -> FilePath  -- ^ Path to the test file
             -> Assertion -- ^ Test result as a HUnit Assertion

-- HUnit assertion: file exists
assertExists :: FilePath -> Sh ()
assertExists f = test_f f >>= liftIO . assertBool msg
  where msg = "File " ++ encodeString f ++ " does not exists"

-- HUnit assertion: cleaning removed all files
assertEmpty :: FilePath -> Sh ()
assertEmpty path = do
  actual <- ls path
  let errMsg = "Directory not empty: " ++ show actual
  liftIO $ assertEqual errMsg [] actual
