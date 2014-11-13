module Main (main) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Control.Monad (forM_)
import Filesystem.Path (filename, basename)
import Filesystem.Path.CurrentOS (encodeString)
import Prelude hiding (FilePath)
import Shelly
import Test.Framework (htfMain)

import TestUtils
import PygmentsTests
import RegressionTests
import JavaTests

main = htfMain [ exitCodeTests, blackBoxTests
               , RegressionTests.all, PygmentsTests.all, JavaTests.all ]

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

-- | Helper function that runs bnfc with the context's options
tcBnfc :: TestContext -> FilePath -> Sh ()
tcBnfc context grammar = run_ "bnfc" args
  where args = tcBnfcOptions context ++ [toTextArg grammar]


settings :: [TestContext]
settings =
  [ base { tcName = "Haskell"
         , tcBnfcOptions = ["--haskell", "-m"] }
  , base { tcName = "Haskell/GADT"
         , tcBnfcOptions = ["--haskell-gadt", "-m"] }
  , base { tcName = "Haskell/CNF"
         , tcBnfcOptions = ["--haskell", "--cnf", "-m"]
         , tcRun = const (cmd ("." </> "TestCNF")) }
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
  [ ( examples</>"cpp"</>"cpp.cf"
    , [ examples</>"cpp"</>"example.cpp"] )

  , ( examples</>"GF"</>"gf.cf"
    , [ examples</>"GF"</>"example.gf"] )

  , ( examples</>"OCL"</>"OCL.cf"
    , [ examples</>"OCL"</>"example.ocl"] )

  , ( examples</>"prolog"</>"Prolog.cf"
    , [ examples</>"prolog"</>"small.pl"
      , examples</>"prolog"</>"simpsons.pl" ] )

  , ( examples</>"C"</>"C.cf"
    , [ examples</>"C"</>"runtime.c"
      , examples</>"C"</>"koe2.c" ] )

  , ( examples</>"C"</>"C4.cf"
    , [ examples</>"C"</>"koe2.c"])

  , ( examples</>"C"</>"C_with_delimiters.cf"
    , [ examples</>"C"</>"small.c" ] )
      -- , examples</>"C"</>"core.c" ] ) -- Fail with CNF!!!

  , ( examples</>"Javalette"</>"JavaletteLight.cf"
    , [examples</>"Javalette"</>"koe.jll"])

  , ( examples</>"LBNF"</>"LBNF.cf"
    , [examples</>"LBNF"</>"LBNF.cf"])

  -- , ( examples</>"Java"</>"java.cf", [] ) -- Cannot be used for testing as
  -- it has duplicate names

  , ( examples</>"Calc.cf", [] )
  , ( examples</>"fstStudio.cf", [] )
  ]
  where examples = ".."</>"examples"

-- A shelly function that, given a test context and a pair grammar+example,
-- runs a complete test in a temp directory
testScript :: TestContext -> (FilePath, [FilePath]) -> Sh ()
testScript context (grammar, examples) = withTmpDir $ \tmp -> do
    cp grammar tmp
    forM_ examples $ flip cp tmp
    cd tmp
    tcBnfc context (filename grammar)
    tcMake context
    forM_ examples $ \example -> do
      readfile (filename example) >>= setStdin
      tcRun context language
  where language = toTextArg (basename grammar)

blackBoxTests :: Test
blackBoxTests =
    makeTestSuite "Black box tests" $ map makeTestSuiteForContext settings
  where makeTestSuiteForContext c =
            makeTestSuite (tcName c) $ map (makeOneTest c) testData
        makeOneTest tc td = makeShellyTest (getLanguage td) $ testScript tc td
        getLanguage (grammar,_) = encodeString (filename grammar)


exitCodeTests :: Test
exitCodeTests = makeTestSuite "Exit code" $ map testExitCode settings
  where
    testExitCode s = makeShellyTest (tcName s) $
        withTmpDir $ \tmp -> do
            cd tmp
            writefile "Abra.cf" "F. C ::= \"abracadabra\";"
            tcBnfc s "Abra.cf"
            tcMake s
            setStdin "bad"
            errExit False $ tcRun s "Abra"
            lastExitCode >>= assertEqual 1
