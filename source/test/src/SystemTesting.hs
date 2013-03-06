{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module SystemTesting where

import Test.HUnit (assertBool, (@=?), Assertion)
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

-- | First we define the backends that we want to test BNFC with.
-- A backend is defined as its name, a list of options to pass to bnfc
-- and a command that runs the generated test program
-- data Backend = Backend String Text (String -> Sh ())
-- backends :: [Backend]
-- backends =
--   [ Backend "Haskell" "-haskell" (cmd . decodeString . ("./Test" ++))
--   , Backend "Java"    "-java"    (\n -> cmd "java" (n </> "Test")) ]
-- 

-- | Next, we define the test cases: a cf grammar, and an input file
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
  -- it looks like cabal doesn't set the binbir variable proserly
  -- when running tests. We need to but the hard coded path instead.
  bnfcPath <- shelly $ absPath ( "dist"</>"build"</>"bnfc"</>"bnfc")
  cases <- testCases
  -- Then we build a list of test groups (one for each backend)
  -- using the List monad
  return $ do 
    testC <- cases
    let name = testName testC
    let (cf,test) = testC
    return $ testCase name (backend bnfcPath cf test)

-- type BNFC = Text -> Text -> FilePath -> Sh Text
--

-- | A backend test the given test file (source code corresponding to the
-- grammar) given a grammar and a binary of bnfc.
-- All path can be assumed to be absolute.
type Backend = FilePath  -- ^ Path to the bnfc binary under test
             -> FilePath  -- ^ Path to the cf grammar
             -> FilePath  -- ^ Path to the test file
             -> Assertion -- ^ Test result as a HUnit Assertion

-- cBAckend :: Backend
-- cBackend bnfcBin cfFile testFile =
--   shelly $ print_commands True $ withTmpDir $ \temp -> do
--     cd temp
--     -- Preconditions: thesting for the existence of the input files
--     assertExists cfFile
--     assertExists testFile
--     -- TODO test existance and executability of bnf
--     bnfc "-m" "-c" cfFile
--     make
--     -- Now we run the test programme, passing the content of
--     -- the test source file on stdin
--     -- Note that we don't print stdout because it can be very verbose
--     -- and we only rely on the status code returned by the test
--     -- program to decide if the test passes or fails
--     readFile testFile >>= setStdin
--     cmd "./test"
--   where make = cmd "make"
--         bnfc = cmd bnfcBin
-- 
-- 
-- mkTest :: BNFC -> Backend -> TestBundle -> Test
-- mkTest bnfc (Backend name language runner) bundle = testCase (testName bundle) $
--   shelly $ print_commands True $
--     withTmpDir $ \temp -> do
--       -- Preconditions: thesting for the existence of the input files
--       assertExists (testGrammar bundle)
--       assertExists (testInput bundle)
--       -- Read the test file. We need to do it before cd-ing to the temp
--       -- directory just in case the path to the file is not absolute.
--       input <- readfile (testInput bundle)
--       -- for the same reasons (relative path) we copy the cf file to the
--       -- temp directory
--       cp (testGrammar bundle) temp
--       -- now we can move to the temporary directory
--       cd temp
--       let cfFile = filename (testGrammar bundle)
--       -- we run bnfc and make to build the test programme
--       bnfc "-m" language cfFile
--       make
--       -- Now we run the test programme, passing the content of
--       -- the test source file on stdin
--       -- Note that we don't print stdout because it can be very verbose
--       -- and we only rely on the status code returned by the test
--       -- program to decide if the test passes or fails
--       setStdin input
--       print_stdout False $ runner (testName bundle)
--   where make = cmd "make"
-- 

-- HUnit assertion: file exists
assertExists :: FilePath -> Sh ()
assertExists f = test_f f >>= liftIO . assertBool msg
  where msg = "File " ++ encodeString f ++ " does not exists"


