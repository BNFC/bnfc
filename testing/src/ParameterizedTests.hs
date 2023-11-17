{-# LANGUAGE CPP #-}

{- ----------------------------------------------------------------------------
 - PARAMETERIZED TESTS
 -
 - This test-suite contains generic tests that are valid expectations for any
 - backend. They take a parameter that defines the backend under test and how
 - it should behave (how to build the code or run the test program.)
 - -}

module ParameterizedTests where

import Control.Monad (forM_)
import Data.Maybe    (mapMaybe)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as Text

import Text.Regex.Posix ((=~))
import System.FilePath (takeBaseName, takeFileName, dropExtension, replaceExtension)

import Shelly
  ( Sh, ShellCmd, (</>)
  , absPath, appendfile
  , canonicalize, cd, cp, cmd
  , echo, errExit
  , lastExitCode --, lastStderr
  , ls
  , readfile, run_
  , setStdin
  , test_f
#if MIN_VERSION_shelly(1,12,0)
  , toTextArgs
#else
  , toTextArg
#endif
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
  [ distcleanTest  params ] :
  testCases params :
  [ exitCodeTest   params ] :
  [ entrypointTest params ] :
  [ exampleTests   params ] :
  []

-- | This parameterized test is called first.
--   Use it while working in connection with a certain test case. (For quicker response.)
current :: Test
-- current = currentExampleTest
current = currentRegressionTest
-- current = layoutTest

currentExampleTest :: Test
currentExampleTest = makeTestSuite "Current parameterized test" $
  mapMaybe (`exampleTest` (exampleGrammars !! 3)) parameters

currentRegressionTest :: Test
currentRegressionTest = makeTestSuite "Current parameterized test" $
  map (`makeTestCase` ("regression-tests" </> cur)) parameters
  where
  -- cur = "comments"
  -- cur = "358_MixFixLists"
  -- cur = "289_LexerKeywords"
  -- cur = "249_unicode"
  cur = "266_define"
  -- cur = "235_SymbolsOverlapTokens"
  -- cur = "202_comments"
  -- cur = "278_Keywords"
  -- cur = "256_Regex"
  -- cur = "70_WhiteSpaceSeparator"
  -- cur = "204_InternalToken"
  -- cur = "222_IntegerList"
  -- cur = "194_layout"
  -- cur = "210_NumberedCatWithoutCoerce"

-- | Layout currently only works for Haskell (even Agda) and Haskell/GADT.
layoutTest :: Test
layoutTest = makeTestSuite "Layout parsing test" $ concat
  [ map (`makeTestCase` ("regression-tests" </> "399_TopLayoutStop")) $
    [ haskellFunctorParameters
    ]
  , map (`makeTestCase` ("regression-tests" </> "356_LayoutSnocList")) $
    [ haskellParameters
    ]
  , map (`makeTestCase` ("regression-tests" </> "194_layout")) $
    [ haskellGADTParameters
    , haskellAgdaParameters
    ]
  , map (`makeTestCase` ("regression-tests" </> "352_TopLayoutOnly")) $
    [ haskellParameters
    ]
  , let p = haskellFunctorParameters
    in  [ makeTestSuite (tpName p) $ mapMaybe (exampleTest p) layoutExamples ]
  ]

-- #254 is now covered by fail-lbnf/254-empty-input
-- -- | BNFC should not proceed if grammar does not define any rules.
-- noRulesTest :: TestParameters -> Test
-- noRulesTest params = do
--   makeShellyTest "#254: BNFC should raise error if grammar contains no rules" $
--     withTmpDir $ \ tmp -> do
--       cd tmp
--       writefile "Empty.cf" "-- This file contains no rules"
--       assertExitCode 1 $ tpBnfc params "Empty.cf"
--       err <- lastStderr
--       unless (Text.isInfixOf "ERROR" err) $
--         assertFailure "Expected BNFC to die with ERROR message."

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
    makeTestSuite "Examples" $ mapMaybe (exampleTest params) exampleGrammars
  --     filter (not . excluded) exampleGrammars
  -- where
  --   excluded :: Example -> Bool
  --   excluded (Example' exclude _ _) = any (tpName params =~) exclude

-- | Construct a test from an example grammar and test inputs,
--   unless the test parameters are contained in the exclusion list
--   or not contained in the inclusion list.
exampleTest :: TestParameters -> Example -> Maybe Test
exampleTest params (Example' limit grammar examples)
  | Excluded exclude <- limit,       any (tpName params =~) exclude = Nothing
  | Included include <- limit, not $ any (tpName params =~) include = Nothing
  | otherwise = Just $
        let lang = takeBaseName grammar in
        makeShellyTest lang $ withTmpDir $ \tmp -> do
            cp grammar tmp
            forM_ examples $ flip cp tmp
            cd tmp
            tpBnfc params (takeFileName grammar)
            tpBuild params
            forM_ examples $ \example ->
                tpRunTestProg params lang [takeFileName example]

-- | To test certain grammatical constructions or interactions between rules,
-- test grammar can be created under the regression-tests directory,
-- together with valid and invalid inputs.
testCases :: TestParameters -> [Test]
testCases params =
    map (makeTestCase params) $
      map ("regression-tests/" ++) $
        [ "266_define"
        , "358_MixFixLists"
        , "235_SymbolsOverlapTokens"
        , "278_Keywords"
        , "256_Regex"
        , "222_IntegerList"
        , "70_WhiteSpaceSeparator"
        , "202_comments"
        , "210_NumberedCatWithoutCoerce"
        , "204_InternalToken"
        , "249_unicode"
        , "289_LexerKeywords"
        , "100_coercion_lists"
        , "comments"
        , "149"
        ]

makeTestCase :: TestParameters -> FilePath -> Test
makeTestCase params dir =
        makeShellyTest (mkTitle dir) $ withTmpDir $ \tmp -> do
            dir <- absPath dir
            dirContents <- ls dir  -- Note: these are absolute filenames!
            cd tmp
            echo "§ Generate"
            let lbnfFile = head $ filter (matchFilePath ".*[.]cf$") dirContents
            let testFile = takeBaseName lbnfFile
            tpBnfc params lbnfFile
            echo "§ Build"
            tpBuild params
            echo "§ Run"
            let good = filter (matchFilePath "good[0-9]*[.]in$") dirContents
            forM_ good $ \f -> do
                output <- tpRunTestProg params testFile [f]
                goldExists <- test_f (replaceExtension f "out")
                when goldExists $ do
                    gold <- readfile (replaceExtension f "out")
                    let (_, goldLT) = parseOutput gold
                        (_, actualLT) = parseOutput output
                    assertEqual goldLT actualLT
            let bad = filter (matchFilePath "bad[0-9]*[.]in$") dirContents
            forM_ bad $ \f -> do
                errExit False $ tpRunTestProg params testFile [f]
                lastExitCode >>= assertEqual 1
  where
    mkTitle dir = tpName params ++ ":" ++ takeFileName dir

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
    assertEqual [cfFile] (map takeFileName dContents)

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
    tpRunTestProg :: FilePath -> [FilePath] -> Sh Text
  }

baseParameters :: TestParameters
baseParameters =  TP
  { tpName        = undefined
  , tpBnfcOptions = undefined
  , tpBuild       = tpMake
  , tpRunTestProg = \ lang args -> do
      bin <- baseTestProg lang
      cmd bin args
  }

-- | Get the test binary.
baseTestProg :: FilePath -> Sh FilePath
baseTestProg lang = canonicalize ("." </> ("Test" <> Text.pack lang))

haskellParameters :: TestParameters
haskellParameters = TP
  { tpName = "Haskell"
  , tpBnfcOptions = ["--haskell"]

  , tpBuild = do
      cmd "hlint"
        "-i" "Avoid lambda"
        "-i" "Avoid lambda using `infix`"
        "-i" "Eta reduce"
        "-i" "Redundant bracket"
        "-i" "Redundant lambda"
        "-i" "Redundant $"
        "-i" "Use camelCase"
        "-i" "Use newtype instead of data"
        "-i" "Use fmap"
        -- "-i" "Unused LANGUAGE pragma"
        "."
      -- cmd "ghc" "-XNoImplicitPrelude" "-Wall" "-Werror" . (:[]) =<< findFileRegex "Abs.*\\.hs$"
      haskellBuild
      -- cmd "ghc" "-XNoImplicitPrelude" "-Wall" "-Werror" . (:[]) =<< findFileRegex "Skel.*\\.hs$"

  , tpRunTestProg = haskellRunTestProg
  }

haskellFunctorParameters :: TestParameters
haskellFunctorParameters = haskellParameters
  { tpName        = "Haskell (with functor)"
  , tpBnfcOptions = ["--haskell", "--functor"]
  }

haskellGADTParameters :: TestParameters
haskellGADTParameters = TP
  { tpName = "Haskell/GADT"
  , tpBnfcOptions = ["--haskell-gadt"]
  , tpBuild       = haskellBuild
  , tpRunTestProg = haskellRunTestProg
  }

haskellAgdaParameters :: TestParameters
haskellAgdaParameters = haskellGADTParameters  -- TODO: use haskellParameters
  { tpName = "Haskell & Agda"
  , tpBnfcOptions = ["--haskell", "--agda", "--functor"]
  }

haskellAgdaFunctorParameters :: TestParameters
haskellAgdaFunctorParameters = haskellGADTParameters  -- TODO: use haskellParameters
  { tpName = "Haskell & Agda (with --functor)"
  , tpBnfcOptions = ["--haskell", "--agda", "--functor"]
  }

-- | Invoke the Makefile with GHC-specific options.
haskellBuild :: Sh ()
haskellBuild = tpMake [ "GHC_OPTS=-XNoImplicitPrelude -Wall -Werror" ]

-- | Haskell backend: default command for running the test executable with the given arguments.
haskellRunTestProg :: FilePath -> [FilePath] -> Sh Text
haskellRunTestProg _lang args = do
      -- cmd "echo" "Looking for Test* binary"
      -- -- can't print anything here because then the setStdin input is used up here
      bin <- findFileRegex "Test[^.]*$"
      -- cmd "echo" "Running" bin  -- ditto
      -- print_stdout True $ print_stderr True $ do
      cmd bin args

parameters :: [TestParameters]
parameters = concat
  [ []
    -- OCaml/Menhir
  , [ ocaml { tpName = "OCaml/Menhir"
            , tpBnfcOptions = ["--ocaml", "--menhir"] }
    ]
    -- OCaml
  , [ ocaml ]
    -- Functor (Haskell & Agda)
  , [ haskellAgdaFunctorParameters]
    -- C++ (extras)
  , [ cBase { tpName = "C++ (with line numbers)"
            , tpBnfcOptions = ["--cpp", "-l"] }
    , cBase { tpName = "C++ (with namespace)"
            , tpBnfcOptions = ["--cpp", "-p foobar"] }
    ]
    -- C
  , [ TP { tpName = "C"
         , tpBnfcOptions = ["--c"]
         , tpBuild = do
             let flags = "CC_OPTS=-Wstrict-prototypes -Werror"
             tpMake [flags]
             tpMake [flags, "Skeleton.o"]
         , tpRunTestProg = \ lang args -> do
             bin <- baseTestProg lang
             cmd bin args
             -- Facility to check for memory leaks
             -- cmd "valgrind" $
             --     "--leak-check=full"  :
             --     "--error-exitcode=1" :
             --     "--errors-for-leak-kinds=definite" :
             --     "--show-leak-kinds=definite" :
             --     bin :
             --     args
         }
    , cBase { tpName = "C (with line numbers)"
            , tpBnfcOptions = ["--c", "--line-numbers"] }

    ]
    -- F#
  , [ fsharp ]
    -- C++ (basic)
  , [ cBase { tpName = "C++ (no STL)"
            , tpBnfcOptions = ["--cpp-nostl"] }
    , cBase { tpName = "C++"
            , tpBnfcOptions = ["--cpp"] }
    ]
    -- Agda
  , [ haskellAgdaParameters ]
    -- Java/ANTLR
  , [ javaParams { tpName = "Java (with antlr)"
                 , tpBnfcOptions = ["--java", "--antlr"] }
    ]
    -- Haskell
  , [ hsParams { tpName = "Haskell (with generic)"
               , tpBnfcOptions = ["--haskell", "--generic"] }
    , hsParams { tpName = "Haskell (with namespace)"
               , tpBnfcOptions = ["--haskell", "-p", "Language", "-d"] }
    ]
    -- Haskell/GADT
  , [ haskellGADTParameters ]
    -- Java (basic)
  , [ javaParams { tpName = "Java"
                 , tpBnfcOptions = ["--java"] }
    ]
    -- Java (extras)
  , [ javaParams { tpName = "Java (with line numbers)"
                 , tpBnfcOptions = ["--java", "-l"] }
    , javaParams { tpName = "Java (with namespace)"
                 , tpBnfcOptions = ["--java", "-p", "my.stuff"] }
    , javaParams { tpName = "Java (with jflex)"
                 , tpBnfcOptions = ["--java", "--jflex"] }
    , javaParams { tpName = "Java (with jflex and line numbers)"
                 , tpBnfcOptions = ["--java", "--jflex", "-l"] }
    ]
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
            cmd "javac" . (:[]) =<< findFile "VisitSkel.java"
        , tpRunTestProg = \ _lang args -> do
            class_ <- dropExtension <$> findFile "Test.class"
            cmd "java" $ "-Xss16M" : class_ : args
        }
    ocaml =  TP
        { tpName        = "OCaml"
        , tpBuild       = tpMake ["OCAMLCFLAGS=-safe-string"]
        , tpBnfcOptions = ["--ocaml"]
        , tpRunTestProg = haskellRunTestProg
        }
    fsharp = TP
        { tpName        = "F#"
        , tpBuild       = do
            cmd "dotnet" ["build"]
        , tpBnfcOptions = ["--fsharp"]
        , tpRunTestProg = \ _lang args -> do
            cmd "dotnet" $ "run" : "--" : args
        }

-- | Helper function that runs bnfc with the context's options and an
--   option to generate 'tpMakefile'.
--   It will simply invoke the bnfc that is in the system's path.

tpBnfc :: TestParameters -> FilePath -> Sh ()
tpBnfc params grammar = run_ "bnfc" args
  where
#if MIN_VERSION_shelly(1,12,0)
  args = toTextArgs ("-m" <> tpMakefile) ++ tpBnfcOptions params ++ toTextArgs grammar
#else
  args = ["-m" <> toTextArg tpMakefile] ++ tpBnfcOptions params ++ [toTextArg grammar]
#endif

-- | Default test parameter specifying the makefile name to be used
--   by 'tpBnfc' and 'tpMake'.
tpMakefile :: FilePath
tpMakefile = "MyMakefile"

-- | Helper function that runs @make@ using 'tpMakefile' as makefile.
tpMake :: ShellCmd result => result
tpMake = cmd "make" [ "-f" , tpMakefile ]

-- | Helper function that runs the command for removing generated files.
tpDistclean :: Sh ()
tpDistclean = tpMake "distclean"
