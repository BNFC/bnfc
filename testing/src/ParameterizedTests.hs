{- ----------------------------------------------------------------------------
 - PARAMETERIZED TESTS
 -
 - This test-suite contains generic tests that are valid expectations for any
 - backend. They take a parameter that defines the backend under test and how
 - it should behave (how to build the code or run the test program.)
 - -}
module ParameterizedTests where

import Control.Monad (forM_)
import Data.Functor
import Data.Monoid ((<>))
import Data.Text (Text)
import Filesystem.Path (filename, dropExtension, basename, replaceExtension)
import Filesystem.Path.CurrentOS (encodeString)
import Prelude hiding (FilePath)
import Shelly
  ( FilePath, Sh, (</>)
  , absPath, appendfile
  , canonicalize, cd, cp, cmd
  , echo, errExit
  , lastExitCode, ls
  , readfile, run_
  , setStdin
  , test_f, toTextArg
  , when, withTmpDir, writefile
  )

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
            writefile "Abra.cf" ";; F. C ::= \"abracadabra\" ;"  -- leading semicolon for #215
            tpBnfc params "Abra.cf"
            tpBuild params
            setStdin "bad"
            errExit False $ tpRunTestProg params "Abra" []
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
        tpRunTestProg params "foobar" []
        -- reject bar
        setStdin "bar"
        errExit False $ tpRunTestProg params "foobar" []
        lastExitCode >>= assertEqual 1

-- Runs BNFC on the example grammars, build the generated code and, if
-- example in the grammar's language are available, tries to parse the
-- examples with the generated test program.
exampleTests :: TestParameters -> Test
exampleTests params =
    makeTestSuite "Examples" $ map exampleTest exampleGrammars
  where
    exampleTest (grammar, examples) =
        let lang = encodeString (basename grammar) in
        makeShellyTest lang $ withTmpDir $ \tmp -> do
            cp grammar tmp
            forM_ examples $ flip cp tmp
            cd tmp
            tpBnfc params (filename grammar)
            tpBuild params
            forM_ examples $ \example ->
                tpRunTestProg params (toTextArg lang) [filename example]

-- | To test certain grammatical constructions or interractions between rules,
-- test grammar can be created under the regression-tests directory,
-- together with valid and invalid inputs.
testCases :: TestParameters -> [Test]
testCases params =
    map makeTestCase
        [ "regression-tests/#100_coercion_lists"
        , "regression-tests/#134_category_named_I"
        , "regression-tests/comments"
        , "regression-tests/#149" ]
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
            good <- filter (matchFilePath "good[0-9]*.in$") <$> ls dir
            forM_ good $ \f -> do
                output <- tpRunTestProg params "test" [f]
                goldExists <- test_f (replaceExtension f "out")
                when goldExists $ do
                    gold <- readfile (replaceExtension f "out")
                    let (_, goldLT) = parseOutput gold
                        (_, actualLT) = parseOutput output
                    assertEqual goldLT actualLT
            bad <- filter (matchFilePath "bad[0-9]*.in$") <$> ls dir
            forM_ bad $ \f -> do
                errExit False $ tpRunTestProg params "test" [f]
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
  , -- | Command for running the test executable with the given arguments
    tpRunTestProg :: Text -> [FilePath] -> Sh Text
  }

parameters :: [TestParameters]
parameters =
  -- Haskell
  [ hsParams { tpName = "Haskell"
             , tpBnfcOptions = ["--haskell", "-m"] }
  , hsParams { tpName = "Haskell (with functor)"
             , tpBnfcOptions = ["--haskell", "--functor", "-m"] }
  , hsParams { tpName = "Haskell (with namespace)"
             , tpBnfcOptions = ["--haskell", "-p", "Language", "-d", "-m"] }
  , hsParams { tpName = "Haskell (with ghc)"
             , tpBnfcOptions = ["--haskell", "--ghc", "-m"] }
  , base { tpName = "Haskell/GADT"
         , tpBnfcOptions = ["--haskell-gadt", "-m"]
         , tpRunTestProg = \_ args -> do bin <- findFileRegex "Test\\w*$"
                                         cmd bin args
         }
  -- OCaml
  , base { tpName = "OCaml"
         , tpBuild = run_ "make" ["OCAMLCFLAGS=-safe-string"]
         , tpBnfcOptions = ["--ocaml", "-m"] }
  -- C
  , cBase { tpName = "C"
          , tpBuild = run_ "make" ["CCFLAGS=-Wstrict-prototypes -Werror"]
          , tpBnfcOptions = ["--c", "-m"] }
  -- C++
  , cBase { tpName = "C++"
          , tpBnfcOptions = ["--cpp", "-m"] }
  , cBase { tpName = "C++ (with namespace)"
          , tpBnfcOptions = ["--cpp", "-p foobar", "-m"] }
  , cBase { tpName = "C++ (no STL)"
          , tpBnfcOptions = ["--cpp-nostl", "-m"] }
  -- Java
  , javaParams { tpName = "Java"
               , tpBnfcOptions = ["--java", "-m"] }
  , javaParams { tpName = "Java (with line numbers)"
               , tpBnfcOptions = ["--java", "-m", "-l"] }
  , javaParams { tpName = "Java (with namespace)"
               , tpBnfcOptions = ["--java", "-p", "my.stuff", "-m"] }
  , javaParams { tpName = "Java (with jflex)"
               , tpBnfcOptions = ["--java", "--jflex", "-m"] }
  , javaParams { tpName = "Java (with jflex and line numbers)"
               , tpBnfcOptions = ["--java", "--jflex", "-m", "-l"] }
  , javaParams { tpName = "Java (with antlr)"
               , tpBnfcOptions = ["--java", "--antlr", "-m"] }
  ]
  where
    base = TP
        { tpName = undefined
        , tpBnfcOptions = undefined
        , tpBuild = run_ "make" []
        , tpRunTestProg = \lang args -> do
            bin <- canonicalize ("." </> ("Test" <> lang))
            cmd bin args
        }
    cBase = base
        { tpBuild = do
            cmd "make"
            cmd "make" "Skeleton.o"
        }
    hsParams = base
        { tpBuild = do
            cmd "hlint" "-i" "Redundant bracket" "-i" "Use camelCase" "-i" "Use newtype instead of data" "-i" "Use fmap" "."
            cmd "make"
            cmd "ghc" =<< findFileRegex "Skel.*\\.hs$"
        , tpRunTestProg = \_ args -> do
            bin <- findFileRegex "Test\\w*$"
            cmd bin args
        }
    javaParams = base
        { tpBuild = do
            cmd "make"
            cmd "javac" =<< findFile "VisitSkel.java"
        , tpRunTestProg = \_ args -> do
            class_ <- dropExtension <$> findFile "Test.class"
            cmd "java" class_ args
        }

-- | Helper function that runs bnfc with the context's options.
--   It will simply invoke the bnfc that is in the system's path.

tpBnfc :: TestParameters -> FilePath -> Sh ()
tpBnfc params grammar = run_ "bnfc" args
  where args = tpBnfcOptions params ++ [toTextArg grammar]
