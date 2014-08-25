{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)
import Filesystem.Path (filename, basename)
import Filesystem.Path.CurrentOS (encodeString)
import Prelude hiding (FilePath)
import Shelly
import System.Exit (exitFailure,exitSuccess)
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.TestTypes
import Test.Framework.TestManager

main = htfMain [blackBoxTests, regressionTests]

{- ----------------------------------------------------------------------------
 - BLACK BOX TESTS
 -
 - In those tests, we generates parsers from some grammars using different
 - backends and check that it compiles and accepts an example program
 - -}

-- A test context is the combination of some bnfc options (that define the
-- backend to use) a build command to compile the resulting parser and a run
-- command to run the test program.
data TestContext = TestContext
  { -- | Name of the test settings, ex 'Haskell with GADT'
    tcName        :: String
  , -- | Options to pass to bnfc
    tcBnfcOptions :: [Text]
  , -- | Command for building the generated test executable
    tcMake        :: Sh ()
  , -- | Command for running the test executable
    tcRun         :: Text -> Sh () }

settings :: [TestContext]
settings =
  [ base { tcName = "Haskell"
         , tcBnfcOptions = ["--haskell", "-m"] }
  , base { tcName = "Haskell/GADT"
         , tcBnfcOptions = ["--haskell-gadt", "-m"] }
  , base { tcName = "Haskell/CNF"
         , tcBnfcOptions = ["--haskell", "--cnf", "-m"]
         , tcRun = \lang -> cmd ("." </> "TestCNF") }
  , base { tcName = "OCaml"
         , tcBnfcOptions = ["--ocaml", "-m"] }
  , base { tcName = "C"
         , tcBnfcOptions = ["--c", "-m"] }
  , base { tcName = "C++"
         , tcBnfcOptions = ["--cpp", "-m"] }
  , base { tcName = "C++ (no STL)"
         , tcBnfcOptions = ["--cpp-nostl", "-m"] }
  , base { tcName = "Java"
         , tcBnfcOptions = ["--java", "-m"]
         , tcRun = \lang -> cmd "java" (lang </> "Test") }
  ]
  where base = TestContext undefined -- name
                           undefined -- bnfc options
                           (run_ "make" []) -- make command
                           (\lang -> cmd ("." </> ("Test" <> lang))) -- run

-- The data to test the different backends with. The first file should be
-- a lbnf grammar and the list contains example programs written in this
-- languague. The list can contain zero, one or more example files. If there
-- is zero, we only test that the grammar is correctly compiled. If there is
-- ore or more, they are fed to the test program and we expect that it exits
-- successfully (i.e. exit code 0).
testData :: [(FilePath, [FilePath])]
testData =
  [ ( "data"</>"cnf"</>"c.cf"
    , [ "data"</>"cnf"</>"small.c" ] )

  , ( "data"</>"c"</>"c.cf"
    , [ "data"</>"c"</>"example.c"] )

  , ( "data"</>"cpp"</>"cpp.cf"
    , [ "data"</>"cpp"</>"example.cpp"] )

  , ( "data"</>"gf"</>"gf.cf"
    , [ "data"</>"gf"</>"example.gf"] )

  , ( "data"</>"ocl"</>"ocl.cf"
    , [ "data"</>"ocl"</>"example.ocl"] )

  , ( "data"</>"lbnf"</>"lbnf.cf"
    , [ "data"</>"lbnf"</>"example.lbnf"] )

  , ( examples</>"prolog"</>"Prolog.cf"
    , [ examples</>"prolog"</>"small.pl"
      , examples</>"prolog"</>"simpsons.pl" ] )

  , ( examples</>"Alfa"</>"Alfa.cf"
    , [ examples</>"Alfa"</>"Sorting.alfa"])

  , ( examples</>"C"</>"C.cf"
    , [ examples</>"C"</>"runtime.c"
      , examples</>"C"</>"koe2.c" ] )

  , ( examples</>"C4.cf"
    , [ examples</>"C"</>"runtime.c"
      , examples</>"C"</>"koe2.c"])

  , ( examples</>"Javalette"</>"JavaletteLight.cf"
    , [examples</>"Javalette"</>"koe.jll"])

  , ( examples</>"LBNF"</>"LBNF.cf"
    , [examples</>"LBNF"</>"LBNF.cf"])
  ]
  where examples = ".."</>"examples"

-- A shelly function that, given a test context and a pair grammar+example,
-- runs a complete test in a temp directory
testScript :: TestContext -> (FilePath, [FilePath]) -> Sh ()
testScript context (grammar, examples) = withTmpDir $ \tmp -> do
    cp grammar tmp
    forM_ examples $ flip cp tmp
    cd tmp
    let args = tcBnfcOptions context ++ [toTextArg (filename grammar)]
    run_ "bnfc" args
    tcMake context
    forM_ examples $ \example -> do
      readfile (filename example) >>= setStdin
      tcRun context language
  where language = toTextArg (basename grammar)

blackBoxTests :: TestSuite
blackBoxTests =
    makeTestSuite "Black box tests" $ map makeTestSuiteForContext settings
  where makeTestSuiteForContext c = CompoundTest $ 
            makeTestSuite (tcName c) $ map (makeOneTest c) testData
        makeOneTest tc td = makeShellyTest (getLanguage td) $ testScript tc td
        getLanguage (grammar,_) = encodeString (filename grammar)

{- ------------------------------------------------------------------------- -
 - REGRESSION TESTS
 - ------------------------------------------------------------------------- -
 -
 - Tests specific to some reported issues -}
regressionTests :: TestSuite
regressionTests = makeTestSuite "Regression tests"
  [ makeShellyTest "#60 Compilation error in Java when a production uses more than one user-defined tokens" $
        withTmpDir $ \tmp -> do
            cd tmp
            writefile "multiple_token.cf" $ T.unlines
                [ "Label. Category ::= FIRST SECOND;"
                , "token FIRST 'a';"
                , "token SECOND 'b';" ]
            cmd "bnfc" "--java" "-m" "multiple_token.cf"
            cmd "make"
  , makeShellyTest "#30 With -d option XML module is not generated inside the directorty" $
        withTmpDir $ \tmp -> do
            cd tmp
            writefile "Test.cf" $ T.unlines
                [ "Start. S ::= S \"a\" ;"
                , "End.   S ::= ;" ]
            cmd "bnfc" "--haskell" "--xml" "-m" "-d" "Test.cf"
            assertFileExists "Test/XML.hs"
            cmd "make"
  , makeShellyTest "#108 C like comments and alex" $
        withTmpDir $ \tmp -> do
            cp "../examples/C.cf" tmp
            cd tmp
            cmd "bnfc" "--haskell" "-m" "C.cf"
            cmd "make"
            setStdin "int a; /* **/ int b; /* */"
            out <- cmd "./TestC"
            liftIO $ print out
            liftIO $ assertBool "Couldn't find `int b` in output"
                                ("int b ;" `T.isInfixOf` out)

   ]

-- ------------------------------------------------------------------------- --
-- TEST UTILS
-- ------------------------------------------------------------------------- --
-- Shortcut function to create a (black box) test from a shelly script
makeShellyTest :: TestID -> Sh () -> Test
makeShellyTest label = makeBlackBoxTest label . shelly . silently

-- A (Shelly) assertion to check the existense of a file
assertFileExists :: FilePath -> Sh ()
assertFileExists p = test_f p >>= liftIO . assertBool errorMessage
  where errorMessage = "Can't find file " ++ encodeString p
