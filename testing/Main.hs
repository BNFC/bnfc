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
import TestData (exampleGrammars)
import PygmentsTests
import RegressionTests
import JavaTests
import CTests
import HaskellTests

main = htfMain [ exitCodeTests
               , blackBoxTests
               , RegressionTests.all
               , PygmentsTests.all
               , JavaTests.all
               , CTests.all
               , HaskellTests.all ]

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
  [ base { tcName = "Haskell/GADT"
         , tcBnfcOptions = ["--haskell-gadt", "-m"]
         , tcRun = \_ -> cmd =<< findFileRegex "Test\\w*$"}
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
  ]
  where base = TestContext undefined -- name
                           undefined -- bnfc options
                           (run_ "make" []) -- make command
                           (\lang -> cmd ("." </> ("Test" <> lang))) -- run


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
            makeTestSuite (tcName c) $ map (makeOneTest c) exampleGrammars
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
