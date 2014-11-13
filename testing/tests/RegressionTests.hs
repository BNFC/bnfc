{- ------------------------------------------------------------------------- -
 - REGRESSION TESTS
 - ------------------------------------------------------------------------- -
 -
 - Tests specific to some reported issues
 - -}
module RegressionTests where

import Data.Text as T
import Test.HUnit (assertBool)
import TestUtils
import Shelly

all = makeTestSuite "Regression tests"
    [ issue60, issue30, issue108, issue110, issue111, issue114, issue113 ]


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

issue108 :: Test
issue108 = makeShellyTest "#108 C like comments and alex" $
    withTmpDir $ \tmp -> do
        cp "../examples/C/C.cf" tmp
        cd tmp
        cmd "bnfc" "--haskell" "-m" "C.cf"
        cmd "make"
        setStdin "int a; /* **/ int b; /* */"
        out <- cmd "./TestC"
        liftIO $ assertBool "Couldn't find `int b` in output"
                            ("int b ;" `T.isInfixOf` out)

issue110 :: Test
issue110 = makeShellyTest "Parse error while building BNFC generated parser #110" $
    withTmpDir $ \tmp -> do
        cp ("regression-tests" </> "110_backslash.cf") tmp
        cd tmp
        cmd "bnfc" "--haskell" "-m" "110_backslash.cf"
        cmd "make"

issue111 :: Test
issue111 =  makeShellyTest "Custom tokens in OCaml" $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "Idents.cf" $ T.unlines
            [ "token UIdent (upper upper*);"
            , "Upper. S ::= UIdent ;" ]
        cmd "bnfc" "--ocaml" "-m" "Idents.cf"
        cmd "make"
        setStdin "VOGONPOETRY"
        out <- cmd "./TestIdents"
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
        output <- cmd "./Test114_listentry"
        assertEqual expected output


issue113 :: Test
issue113 = makeShellyTest "#113 BNFC to Java creates non-compilable code when using user-defined tokens in grammar" $
    withTmpDir $ \tmp -> do
        cp "regression-tests/113_javatokens.cf" (tmp </> "grammar.cf")
        cd tmp
        cmd "bnfc" "--java" "grammar.cf"
        cmd "javac" "grammar/VisitSkel.java"
