module CppTests (all) where

import TestUtils
import Shelly
import Prelude hiding (FilePath, all)
--import TestData (exampleGrammars)

all = makeTestSuite "C++" [issue127]

-- | Issue #127
issue127 :: Test
issue127 = makeShellyTest "Issue #127 " $
    withTmpDir $ \tmp -> do
        cd tmp
        writefile "foobar.cf" "entrypoints Bar;"
        appendfile "foobar.cf" "F. Foo ::= \"foo\" ;"
        appendfile "foobar.cf" "B. Bar ::= \"bar\" ;"
        cmd "bnfc" "--cpp" "-m" "foobar.cf"
        cmd "make"
        -- reject foo
        setStdin "foo"
        errExit False $ cmd "./Testfoobar"
        lastExitCode >>= assertEqual 1
        -- accept bar
        setStdin "bar"
        cmd "./Testfoobar"

