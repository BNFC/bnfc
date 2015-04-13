{- ----------------------------------------------------------------------------
 - PARAMETERIZED TESTS
 -
 - This test-suite contains generic tests that are valid expectations for any
 - backend. They take a parameter that defines the backend under test and how
 - it should behave (how to build the code or run the test program.)
 - -}
module ParameterizedTests where

import Control.Monad (forM_, liftM)
import Data.Monoid ((<>))
import Data.Text (Text)
import Filesystem.Path (filename, dropExtension, basename, replaceExtension)
import Filesystem.Path.CurrentOS (encodeString)
import Prelude hiding (FilePath)
import Shelly

import TestUtils
import TestData
import OutputParser

-- ~~~ TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all :: Test
all = makeTestSuite "Parameterized tests" [allWithParams p | p <- parameters ]

allWithParams :: TestParameters -> Test
allWithParams params = makeTestSuite (tpName params) $
    [ exitCodeTest params
    , entrypointTest params
    , exampleTests params
    ] ++ testCases params


-- This tests checks that when given an invalid input, the generated example
-- application exits with a non-zerro exit code.
exitCodeTest :: TestParameters -> Test
exitCodeTest params =
    makeShellyTest "Test program exits with code 1 on failure" $
        withTmpDir $ \tmp -> do
            cd tmp
            writefile "Abra.cf" "F. C ::= \"abracadabra\";"
            tpBnfc params "Abra.cf"
            tpBuild params
            setStdin "bad"
            errExit False $ tpRunTestProg params "Abra"
            lastExitCode >>= assertEqual 1

-- This tests that the generated parser abide to the entrypoint directive
-- (see issue #127)
entrypointTest :: TestParameters -> Test
entrypointTest params =
    makeShellyTest "Respects entrypoint directive" $ withTmpDir $ \tmp -> do
        cd tmp
        writefile "foobar.cf" "entrypoints Foo;"
        appendfile "foobar.cf" "B. Bar ::= \"bar\" ;"
        appendfile "foobar.cf" "F. Foo ::= \"foo\" Bar ;"
        tpBnfc params "foobar.cf"
        tpBuild params
        -- accept foo bar
        setStdin "foo bar"
        tpRunTestProg params "foobar"
        -- reject bar
        setStdin "bar"
        errExit False $ tpRunTestProg params "foobar"
        lastExitCode >>= assertEqual 1

-- Runs BNFC on the example grammars, build the generated code and, if
-- example in the grammar's language are available, tries to parse the
-- examples with the generated test program.
exampleTests :: TestParameters -> Test
exampleTests params =
    makeTestSuite "Examples" $ map exampleTest exampleGrammars
  where
    exampleTest (grammar, examples) =
        let lang = encodeString (filename grammar) in
        makeShellyTest lang $ withTmpDir $ \tmp -> do
            cp grammar tmp
            forM_ examples $ flip cp tmp
            cd tmp
            tpBnfc params (filename grammar)
            tpBuild params
            forM_ examples $ \example -> do
                readfile (filename example) >>= setStdin
                tpRunTestProg params (toTextArg $ basename grammar)

-- | To test certain grammatical constructions or interractions between rules,
-- test grammar can be created under the regression-tests directory,
-- together with valid and invalid inputs.
testCases :: TestParameters -> [Test]
testCases params = do
    map makeTestCase
        [ "regression-tests/#100_coercion_lists"
        , "regression-tests/#134_category_named_I" ]
  where
    mkTitle dir = encodeString (filename dir)
    makeTestCase dir =
        makeShellyTest (mkTitle dir) $ withTmpDir $ \tmp -> do
            dir <- absPath dir
            cd tmp
            echo "§ Generate"
            tpBnfc params (dir </> "test.cf")
            echo "§ Build"
            tpBuild params
            good <- liftM (filter (matchFilePath "good[0-9]*.in$")) (ls dir)
            forM_ good $ \f -> do
                readfile f >>= setStdin
                output <- tpRunTestProg params "test"
                goldExists <- test_f (replaceExtension f "out")
                when goldExists $ do
                    gold <- readfile (replaceExtension f "out")
                    let (_, goldLT) = parseOutput gold
                        (_, actualLT) = parseOutput output
                    assertEqual goldLT actualLT
            bad <- liftM (filter (matchFilePath "bad[0-9]*.in$")) (ls dir)
            forM_ bad $ \f -> do
                readfile f >>= setStdin
                errExit False $ tpRunTestProg params "test"
                lastExitCode >>= assertEqual 1

-- ~~~ Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Test parameters are the combination of bnfc options (that define the
-- backend to use) a build command to compile the resulting parser and a run
-- command to run the test program.
data TestParameters = TP
  { -- | Name of the test settings, ex 'Haskell with GADT'
    tpName        :: String
  , -- | Options to pass to bnfc
    tpBnfcOptions :: [Text]
  , -- | Command for building the generated test executable
    tpBuild       :: Sh ()
  , -- | Command for running the test executable
    tpRunTestProg :: Text -> Sh Text
  }

parameters :: [TestParameters]
parameters =
  [ hsParams { tpName = "Haskell" , tpBnfcOptions = ["--haskell", "-m"] }
  , hsParams { tpName = "Haskell (with functor)"
             , tpBnfcOptions = ["--haskell", "--functor", "-m"] }
  , hsParams { tpName = "Haskell (with namespace)"
             , tpBnfcOptions = ["--haskell", "-p", "Language", "-d", "-m"] }
  , base { tpName = "Haskell/GADT"
         , tpBnfcOptions = ["--haskell-gadt", "-m"]
         , tpRunTestProg = \_ -> cmd =<< findFileRegex "Test\\w*$"}
  , base { tpName = "OCaml"
         , tpBnfcOptions = ["--ocaml", "-m"] }
  , base { tpName = "C"
         , tpBnfcOptions = ["--c", "-m"] }
  , base { tpName = "C++"
         , tpBnfcOptions = ["--cpp", "-m"] }
  , base { tpName = "C++ (no STL)"
         , tpBnfcOptions = ["--cpp-nostl", "-m"] }
  , javaParams { tpName = "Java"
               , tpBnfcOptions = ["--java", "-m"] }
  , javaParams { tpName = "Java (with namespace)"
               , tpBnfcOptions = ["--java", "-p","my.stuff", "-m"] }
  , javaParams { tpName = "Java (with jflex)"
               , tpBnfcOptions = ["--java", "--jflex", "-m"] }
  ]
  where
    base = TP undefined {-name-} undefined {-bnfc options-}
                       (run_ "make" []) {-build-}
                       (\lang -> cmd ("." </> ("Test" <> lang))) {-run-}
    hsParams = base
        { tpBuild =
            do { cmd "make" ; cmd "ghc" =<< findFileRegex "Skel.*\\.hs$" }
        , tpRunTestProg =
            const (cmd =<< findFileRegex "Test\\w*$" ) }
    javaParams = base
        { tpBuild =
            do { cmd "make" ; cmd "javac" =<< findFile "VisitSkel.java" }
        , tpRunTestProg =
            const (cmd "java" =<< liftM dropExtension (findFile "Test.class"))
        }

-- | Helper function that runs bnfc with the context's options
tpBnfc :: TestParameters -> FilePath -> Sh ()
tpBnfc params grammar = run_ "bnfc" args
  where args = tpBnfcOptions params ++ [toTextArg grammar]
