{- ------------------------------------------------------------------------- -
 - REGRESSION TESTS
 - ------------------------------------------------------------------------- -
 -
 - Tests specific to some reported issues
 - -}
module RegressionTests (all) where

import qualified Data.Text as T
import Test.HUnit (assertBool)
import TestUtils
import Shelly
import Prelude hiding (all)

all = makeTestSuite "Regression tests"
    [ issue30, issue31
    , issue60
    , issue108, issue110, issue111, issue114, issue113
    , issue127, issue128
    , issue151, issue159
    , issue170a, issue170b
    , issue172
    , issue186
    ]

issue30 :: Test
issue30 = makeShellyTest "#30 With -d option XML module is not generated inside the directorty" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "Test.cf" $ T.unlines
            [ "Start. S ::= S \"a\" ;"
            , "End.   S ::= ;" ]
        cmd "bnfc" "--haskell" "--xml" "-m" "-d" "Test.cf"
        assertFileExists "Test/XML.hs"
        cmd "make"

-- | Issue #31
issue31 :: Test
issue31 = makeShellyTest "#31 Problem with multiples `rules` declaration with a common prefix" $
    withTmpDir $ \tmp -> do
        cp "regression-tests/31_multirules.cf" (tmp </> "grammar.cf")
        cd tmp
        cmd "bnfc" "--java" "grammar.cf"

issue60 :: Test
issue60 = makeShellyTest "#60 Compilation error in Java when a production uses more than one user-defined tokens" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "multiple_token.cf" $ T.unlines
            [ "Label. Category ::= FIRST SECOND;"
            , "token FIRST 'a';"
            , "token SECOND 'b';" ]
        cmd "bnfc" "--java" "-m" "multiple_token.cf"
        cmd "make"

issue108 :: Test
issue108 = makeShellyTest "#108 C like comments and alex" $
    withTmpDir $ \tmp -> do
        cp "../examples/C/C.cf" tmp
        cd tmp
        cmd "bnfc" "--haskell" "-m" "C.cf"
        cmd "make"
        setStdin "int a; /* **/ int b; /* */"
        out <- cmd =<< canonicalize "./TestC"
        liftIO $ assertBool "Couldn't find `int b` in output"
                            ("int b ;" `T.isInfixOf` out)

issue110 :: Test
issue110 = makeShellyTest "#110 Parse error while building BNFC generated parser" $
    withTmpDir $ \tmp -> do
        cp ("regression-tests" </> "110_backslash.cf") tmp
        cd tmp
        cmd "bnfc" "--haskell" "-m" "110_backslash.cf"
        cmd "make"

issue111 :: Test
issue111 =  makeShellyTest "#111 Custom tokens in OCaml" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "Idents.cf" $ T.unlines
            [ "token UIdent (upper upper*);"
            , "Upper. S ::= UIdent ;" ]
        cmd "bnfc" "--ocaml" "-m" "Idents.cf"
        cmd "make"
        setStdin "VOGONPOETRY"
        out <- cmd =<< canonicalize "./TestIdents"
        liftIO $ print out

issue114 :: Test
issue114 = makeShellyTest "#114 List category as entry point" $
    withTmpDir $ \tmp -> do
        cp "regression-tests/114_listentry.cf" tmp
        cp "regression-tests/114_listentry.in" tmp
        input <- readfile "regression-tests/114_listentry.in"
        expected <- readfile "regression-tests/114_listentry.out"
        cd tmp
        cmd "bnfc" "--haskell" "-m" "114_listentry.cf"
        cmd "make"
        setStdin input
        output <- cmd =<< canonicalize "./TestListentry"
        assertEqual expected output

issue113 :: Test
issue113 = makeShellyTest "#113 BNFC to Java creates non-compilable code when using user-defined tokens in grammar" $
    withTmpDir $ \tmp -> do
        cp "regression-tests/113_javatokens.cf" (tmp </> "grammar.cf")
        cd tmp
        cmd "bnfc" "--java" "grammar.cf"
        cmd "javac" "grammar/VisitSkel.java"

-- | Issue #127
issue127 :: Test
issue127 = makeShellyTest "#127 Problems with C and entrypoints" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "foobar.cf" "entrypoints Bar;"
        appendfile "foobar.cf" "F. Foo ::= \"foo\" ;"
        appendfile "foobar.cf" "B. Bar ::= \"bar\" ;"
        cmd "bnfc" "--c" "-m" "foobar.cf"
        cmd "make"
        -- reject foo
        setStdin "foo"
        assertExitCode 1 $ cmd =<< canonicalize "./Testfoobar"
        -- accept bar
        setStdin "bar"
        assertExitCode 0 $ cmd =<< canonicalize "./Testfoobar"

-- | Issue #128
issue128 :: Test
issue128 = makeShellyTest "#128 Cannot use B as a constructor in haskell" $
    withTmpDir $ \tmp -> do
        cp "regression-tests/128_bar.cf" (tmp </> "grammar.cf")
        cd tmp
        cmd "bnfc" "--haskell" "-m" "grammar.cf"
        cmd "make"

-- | Issue # 151
issue151 :: Test
issue151 = makeShellyTest "#151 Shouldn't print all categories in error message" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "test.cf" "Foo. Bar ::= Baz"
        errExit False $ do
          cmd "bnfc" "test.cf"
          code <- lastExitCode
          err <- lastStderr
          assertEqual code 1
          let expectedErr = T.unlines
                  [ "no production for Baz, appearing in rule"
                  , "    Foo. Bar ::= Baz", "" ]
          assertEqual expectedErr err

-- |Issue #172
issue159 :: Test
issue159 = makeShellyTest "#159 String rendering in Java does not work" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "issue.cf" $ T.unlines
            [ "One . BugOne ::= \"the_following_is_a_quoted_string\" String;" ]
        cmd "bnfc" "-m" "--java" "issue.cf"
        cmd "make"
        let input = "the_following_is_a_quoted_string \"here I am\""
        setStdin input
        out <- cmd "java"  "issue/Test"
        liftIO $ assertBool "Invalid output" (input `T.isInfixOf` out)

issue170a :: Test
issue170a = makeShellyTest "#170 Module Xml cannot be compiled with GADT backend (--xml)" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "Test.cf" $ T.unlines
            [ "Start. S ::= S \"a\" ;"
            , "End.   S ::= ;" ]
        cmd "bnfc" "--haskell-gadt" "--xml" "-m" "Test.cf"
        cmd "make"


issue170b :: Test
issue170b = makeShellyTest "#170 Module Xml cannot be compiled with GADT backend (--xmlt)" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "Test.cf" $ T.unlines
            [ "Start. S ::= S \"a\" ;"
            , "End.   S ::= ;" ]
        cmd "bnfc" "--haskell-gadt" "--xmlt" "-m" "Test.cf"
        cmd "make"


-- |Issue #172
issue172 :: Test
issue172 = makeShellyTest "#172 Prefixes not generated correctly in CPP" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "Test.cf" $ T.unlines
            [ "Start. S ::= S \"a\" ;"
            , "End.   S ::= ;" ]
        cmd "bnfc" "-m" "--cpp" "-p" "Haskell" "Test.cf"
        cmd "make"

-- | Issue #186
issue186 :: Test
issue186 = makeShellyTest "#186 Rule labels have to start with uppercase char" $
  withTmpDir $ \tmp -> do
      cd tmp
      writefile "Foobar.cf" $ T.unlines
        ["cat . Bar ::= \"cat\" ;"
        ,"dog . Bar ::= \"dog\" ;"
        ,"Cat . Bar ::= \"Cat\" ;"
        ]
      out <- cmd "bnfc" "Foobar.cf"
      let expectedMessageCat = "Warning: bad constructor name cat in cat. Bar ::= \"cat\""
          expectedMessageDog = "Warning: bad constructor name dog in dog. Bar ::= \"dog\""
      liftIO $ assertBool
        "Didn't print out warning for lower case constructors"
        (expectedMessageCat `T.isInfixOf` out &&
         expectedMessageDog `T.isInfixOf` out)
