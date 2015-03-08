module CTests (all) where

import TestUtils
import Shelly
import Prelude hiding (FilePath, all)
import Filesystem.Path (filename, basename)
import TestData (exampleGrammars)

all = makeTestSuite "C backend" [testVisitorSkels, issue127]

-- | Given the path to a grammar file, this will generate c code using bnfc
-- and make sure that the generated Visitor skeleton compiles with gcc.
testVisitorSkel :: FilePath -> Test
testVisitorSkel grammar = makeShellyTest (pathToString (basename grammar)) $
    withTmpDir $ \tmp -> do
        cp grammar tmp
        cd tmp
        cmd "bnfc" "--c" (filename grammar)
        assertFileExists "Skeleton.h"
        assertFileExists "Skeleton.c"
        cmd "gcc" "-c" "Skeleton.c"

-- | Instanciate the test above with all test grammars
testVisitorSkels = makeTestSuite "Compile Skeleton.c" $
    map (testVisitorSkel . fst) exampleGrammars

-- | Issue #127
issue127 :: Test
issue127 = makeShellyTest "Problems with C and entrypoints #127 " $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "foobar.cf" "entrypoints Bar;"
        appendfile "foobar.cf" "F. Foo ::= \"foo\" ;"
        appendfile "foobar.cf" "B. Bar ::= \"bar\" ;"
        cmd "bnfc" "--c" "-m" "foobar.cf"
        cmd "make"
        -- reject foo
        setStdin "foo"
        errExit False $ cmd "./Testfoobar"
        lastExitCode >>= assertEqual 1
        -- accept bar
        setStdin "bar"
        cmd "./Testfoobar"
