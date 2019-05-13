{- ----------------------------------------------------------------------------
 - PARAMETERIZED TESTS
 -
 - This test-suite contains generic tests that are valid expectations for any
 - backend. They take a parameter that defines the backend under test and how
 - it should behave (how to build the code or run the test program.)
 - -}
module ParameterizedTests where

import Control.Monad (forM_, unless)
import Data.Functor
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Filesystem.Path (filename, dropExtension, basename, replaceExtension)
import Filesystem.Path.CurrentOS (encodeString)
import Prelude hiding (FilePath)
import Shelly
  ( FilePath, Sh, ShellCmd, (</>)
  , absPath, appendfile
  , canonicalize, cd, cp, cmd
  , echo, errExit
  , lastExitCode, lastStderr, ls
  , readfile, run_
  , setStdin
  , test_f, toTextArg
  , when, withTmpDir, writefile
  -- , print_stdout, print_stderr
  )

import TestUtils
import TestData
import OutputParser

-- ~~~ TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all :: Test
all = makeTestSuite "Parameterized tests" [allWithParams p | p <- parameters ]

allWithParams :: TestParameters -> Test
allWithParams params = makeTestSuite (tpName params) $ concat $
  [ testCases params
  , map ($ params) $
    [ noRulesTest
    , exitCodeTest
    , entrypointTest
    , exampleTests
    , distcleanTest
    ]
  ]

-- | This parameterized test is called first.
--   Use it while working in connection with a certain test case. (For quicker response.)
current :: Test
current = layoutTest
-- current = makeTestSuite "Current parameterized test" $
--   map (`exampleTest` (exampleGrammars !! 1)) parameters
-- current = makeTestSuite "Current parameterized test" $
--   map (`makeTestCase` ("regression-tests" </> cur)) parameters
--   where
--   cur = "194_layout"
--   -- cur = "210_NumberedCatWithoutCoerce"

-- | Layout currently only works for Haskell (even Agda) and Haskell/GADT.
layoutTest :: Test
layoutTest = makeTestSuite "Layout parsing test" $
  map (`makeTestCase` ("regression-tests" </> "194_layout")) $
  [ haskellAgdaParameters
  , haskellGADTParameters
  ]

-- | BNFC should not proceed if grammar does not define any rules.
noRulesTest :: TestParameters -> Test
noRulesTest params = do
  makeShellyTest "#254: BNFC should raise error if grammar contains no rules" $
    withTmpDir $ \ tmp -> do
      cd tmp
      writefile "Empty.cf" "-- This file contains no rules"
      assertExitCode 1 $ tpBnfc params "Empty.cf"
      err <- lastStderr
      unless (Text.isInfixOf "ERROR" err) $
        assertFailure "Expected BNFC to die with ERROR message."

-- | This tests checks that when given an invalid input, the generated example
-- application exits with a non-zero exit code.
exitCodeTest :: TestParameters -> Test
exitCodeTest params =
    makeShellyTest "Test program exits with code 1 on failure" $
        withTmpDir $ \tmp -> do
            cd tmp
            writefile "Abra.cf" ";; F. C ::= \"abracadabra\" ;"  -- leading semicolon for #215
            tpBnfc params "Abra.cf"
            tpBuild params
            -- setStdin "If this text is printed, then setStdin works\n"
            -- print_stdout True $ run_ "cat" []
            setStdin "abracadabra\n"
            tpRunTestProg params "Abra" []
            setStdin "bad"
            assertExitCode 1 $ tpRunTestProg params "Abra" []

-- | This tests that the generated parser abide to the entrypoint directive
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

-- | Runs BNFC on the example grammars, build the generated code and, if
-- example in the grammar's language are available, tries to parse the
-- examples with the generated test program.
exampleTests :: TestParameters -> Test
exampleTests params =
    makeTestSuite "Examples" $ map (exampleTest params) exampleGrammars

exampleTest :: TestParameters -> Example -> Test
exampleTest params (grammar, examples) =
        let lang = encodeString (basename grammar) in
        makeShellyTest lang $ withTmpDir $ \tmp -> do
            cp grammar tmp
            forM_ examples $ flip cp tmp
            cd tmp
            tpBnfc params (filename grammar)
            tpBuild params
            forM_ examples $ \example ->
                tpRunTestProg params (toTextArg lang) [filename example]

-- | To test certain grammatical constructions or interactions between rules,
-- test grammar can be created under the regression-tests directory,
-- together with valid and invalid inputs.
testCases :: TestParameters -> [Test]
testCases params =
    map (makeTestCase params)
        [ "regression-tests/210_NumberedCatWithoutCoerce"
        , "regression-tests/204_InternalToken"
        , "regression-tests/249_unicode"
        , "regression-tests/#100_coercion_lists"
        , "regression-tests/#134_category_named_I"
        , "regression-tests/comments"
        , "regression-tests/#149"
        ]

makeTestCase :: TestParameters -> FilePath -> Test
makeTestCase params dir =
        makeShellyTest (mkTitle dir) $ withTmpDir $ \tmp -> do
            dir <- absPath dir
            cd tmp
            echo "§ Generate"
            tpBnfc params (dir </> "test.cf")
            echo "§ Build"
            tpBuild params
            echo "§ Run"
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
  where
    mkTitle dir = encodeString (filename dir)

-- | To test that @distclean@ removes all generated files.
distcleanTest :: TestParameters -> Test
distcleanTest params =
  makeShellyTest "distclean removes all generated files" $ withTmpDir $ \tmp -> do
    cd tmp
    let cfFile = "G.cf"
    writefile cfFile "L. C ::= \"t\" ;"
    tpBnfc params cfFile
    tpBuild params
    tpDistclean
    dContents <- ls "."
    assertEqual [cfFile] (map filename dContents)

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

baseParameters :: TestParameters
baseParameters =  TP
  { tpName        = undefined
  , tpBnfcOptions = undefined
  , tpBuild       = tpMake
  , tpRunTestProg = \ lang args -> do
      bin <- canonicalize ("." </> ("Test" <> lang))
      cmd bin args
  }

haskellParameters :: TestParameters
haskellParameters = baseParameters
  { tpName = "Haskell"
  , tpBnfcOptions = ["--haskell"]

  , tpBuild = do
      cmd "hlint"
        "-i" "Redundant bracket"
        "-i" "Use camelCase"
        "-i" "Use newtype instead of data"
        "-i" "Use fmap"
        "."
      tpMake
      cmd "ghc" =<< findFileRegex "Skel.*\\.hs$"

  , tpRunTestProg = haskellRunTestProg
  }

haskellGADTParameters :: TestParameters
haskellGADTParameters = baseParameters
  { tpName = "Haskell/GADT"
  , tpBnfcOptions = ["--haskell-gadt"]
  , tpRunTestProg = haskellRunTestProg
  }

haskellAgdaParameters :: TestParameters
haskellAgdaParameters = haskellParameters
  { tpName = "Haskell & Agda"
  , tpBnfcOptions = ["--haskell", "--agda"]
  }


-- | Haskell backend: default command for running the test executable with the given arguments.
haskellRunTestProg :: Text -> [FilePath] -> Sh Text
haskellRunTestProg _lang args = do
      -- cmd "echo" "Looking for Test* binary"
      -- -- can't print anything here because then the setStdin input is used up here
      bin <- findFileRegex "Test[^.]*$"
      -- cmd "echo" "Running" bin  -- ditto
      -- print_stdout True $ print_stderr True $ do
      cmd bin args

parameters :: [TestParameters]
parameters =
  -- Haskell
  [ haskellAgdaParameters
  , hsParams { tpName = "Haskell (with ghc)"
             , tpBnfcOptions = ["--haskell", "--ghc"] }
  , hsParams { tpName = "Haskell (with functor)"
             , tpBnfcOptions = ["--haskell", "--functor"] }
  , hsParams { tpName = "Haskell (with namespace)"
             , tpBnfcOptions = ["--haskell", "-p", "Language", "-d"] }
  -- Haskell/GADT
  , haskellGADTParameters
  -- OCaml
  , base { tpName = "OCaml"
         , tpBuild = tpMake ["OCAMLCFLAGS=-safe-string"]
         , tpBnfcOptions = ["--ocaml"] }
  -- C
  , cBase { tpName = "C"
          , tpBuild = tpMake ["CCFLAGS=-Wstrict-prototypes -Werror"]
          , tpBnfcOptions = ["--c"] }
  -- C++
  , cBase { tpName = "C++"
          , tpBnfcOptions = ["--cpp"] }
  , cBase { tpName = "C++ (with namespace)"
          , tpBnfcOptions = ["--cpp", "-p foobar"] }
  , cBase { tpName = "C++ (no STL)"
          , tpBnfcOptions = ["--cpp-nostl"] }
  -- Java
  , javaParams { tpName = "Java"
               , tpBnfcOptions = ["--java"] }
  , javaParams { tpName = "Java (with line numbers)"
               , tpBnfcOptions = ["--java", "-l"] }
  , javaParams { tpName = "Java (with namespace)"
               , tpBnfcOptions = ["--java", "-p", "my.stuff"] }
  , javaParams { tpName = "Java (with jflex)"
               , tpBnfcOptions = ["--java", "--jflex"] }
  , javaParams { tpName = "Java (with jflex and line numbers)"
               , tpBnfcOptions = ["--java", "--jflex", "-l"] }
  , javaParams { tpName = "Java (with antlr)"
               , tpBnfcOptions = ["--java", "--antlr"] }
  ]
  where
    base = baseParameters
    hsParams = haskellParameters
    cBase = base
        { tpBuild = do
            tpMake
            tpMake "Skeleton.o"
        }
    javaParams = base
        { tpBuild = do
            tpMake
            cmd "javac" =<< findFile "VisitSkel.java"
        , tpRunTestProg = \_ args -> do
            class_ <- dropExtension <$> findFile "Test.class"
            cmd "java" class_ args
        }

-- | Helper function that runs bnfc with the context's options and an
--   option to generate 'tpMakefile'.
--   It will simply invoke the bnfc that is in the system's path.

tpBnfc :: TestParameters -> FilePath -> Sh ()
tpBnfc params grammar = run_ "bnfc" args
  where args = ["-m" <> toTextArg tpMakefile] ++ tpBnfcOptions params ++ [toTextArg grammar]

-- | Default test parameter specifying the makefile name to be used
--   by 'tpBnfc' and 'tpMake'.
tpMakefile :: FilePath
tpMakefile = "MyMakefile"

-- | Helper function that runs @make@ using 'tpMakefile' as makefile.
tpMake :: ShellCmd result => result
tpMake = cmd "make" "-f" tpMakefile

-- | Helper function that runs the command for removing generated files.
tpDistclean :: Sh ()
tpDistclean = tpMake "distclean"
