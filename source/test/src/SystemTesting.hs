{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Test.HUnit (assertBool, (@=?))
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Filesystem.Path (basename, filename)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Text.Printf (printf)
import Shelly
import Prelude hiding (FilePath)
import Data.Text.Lazy (Text)
import Control.Exception (assert)
import Paths_BNFC (getBinDir)

default (Text)

-- | First we define the backends that we want to test BNFC with.
-- A backend is defined as its name, a list of options to pass to bnfc
-- and a command that runs the generated test program
data Backend = Backend String Text (String -> Sh ())
backends :: [Backend]
backends =
  [ Backend "Haskell" "-haskell" (cmd . decodeString . ("./Test" ++)) ]
--  , Backend "Java"    "-java"    (\n -> cmd "java" (n </> "Test")) ]


-- | Next, we define the test cases: a cf grammar, and an input file
type TestBundle = (FilePath, FilePath)

testName :: TestBundle -> String
testName = encodeString . basename . fst

testGrammar :: TestBundle -> FilePath
testGrammar = fst

testInput :: TestBundle -> FilePath
testInput = snd

testCases :: [TestBundle]
testCases = make
  [ ( "c/c.cf",       "c/test.c" )
  , ( "cpp/cpp.cf",   "cpp/test.cpp" )
  , ( "gf/gf.cf",     "gf/test.gf" )
  , ( "lbnf/lbnf.cf", "lbnf/test.lbnf" )
  , ( "ocl/ocl.cf",   "ocl/test.ocl" ) ]
  where make = map (\(f1,f2) -> ("test/data" </> f1, "test/data" </> f2))

main = testFactory >>= defaultMain

-- | Build The Test Suite
testFactory :: IO [Test]
testFactory = do
  -- first we have to find where bnfc is. To do that, 
  -- we use the getBinDir exposed by cabal and turn the returned
  -- string in a FilePath. Then we concatenate “bnfc” to this
  -- path. This might be a problem on windows though...
  bin <- getBinDir >>= return . decodeString
  let bnfc = cmd (bin </> "bnfc")
  -- Then we build a list of test groups (one for each backend)
  -- using the List monad
  return $ do
    bknd@(Backend name _ _) <- backends
    let tests = [mkTest bnfc bknd tc | tc <- testCases]
    return (testGroup name tests)

type BNFC = Text -> Text -> FilePath -> Sh Text

mkTest :: BNFC -> Backend -> TestBundle -> Test
mkTest bnfc (Backend name language runner) bundle = testCase (testName bundle) $
  shelly $ print_commands True $
    withTmpDir $ \temp -> do
      -- Preconditions: thesting for the existence of the input files
      assertExists (testGrammar bundle)
      assertExists (testInput bundle)
      -- Read the test file. We need to do it before cd-ing to the temp
      -- directory just in case the path to the file is not absolute.
      input <- readfile (testInput bundle)
      -- for the same reasons (relative path) we copy the cf file to the
      -- temp directory
      cp (testGrammar bundle) temp
      -- now we can move to the temporary directory
      cd temp
      let cfFile = filename (testGrammar bundle)
      -- we run bnfc and make to build the test programme
      bnfc "-m" language cfFile
      make
      -- Now we run the test programme, passing the content of
      -- the test source file on stdin
      -- Note that we don't print stdout because it can be very verbose
      -- and we only rely on the status code returned by the test
      -- program to decide if the test passes or fails
      setStdin input
      print_stdout False $ runner (testName bundle)
  where make = cmd "make"


-- HUnit assertion: file exists
assertExists :: FilePath -> Sh ()
assertExists f = test_f f >>= liftIO . assertBool msg
  where msg = "File " ++ encodeString f ++ " does not exists"


