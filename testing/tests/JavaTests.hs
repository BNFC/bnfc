module JavaTests (all) where

import Control.Monad (forM_)
import TestUtils
import Shelly
import Prelude hiding (FilePath, all)
import Filesystem.Path (filename, basename)
import TestData

all = makeTestSuite "Java"
    [ makeTestSuite "default options" (map factory exampleGrammars)
    , issue31
    ]

-- | Test factory
-- Given a grammar files and eventually some example programma parsable with
-- the grammar, this produce a testsuite where code is generated using bnfc and
-- then either
-- - uses 'make' to build the test program
-- - uses javac to build the skeleton file
factory :: Example -> Test
factory (grammar,examples) = makeTestSuite name
    [ makeShellyTest "make test program" $
        withTmpDir $ \tmp -> do
            cp grammar tmp
            forM_ examples $ flip cp tmp
            cd tmp
            cmd "bnfc" "-m" "--java" (toTextArg (filename grammar))
            cmd "make"
            forM_ examples $ \example -> do
                readfile (filename example) >>= setStdin
                cmd "java" (lang </> "Test")
    -- | Given the grammar file, this will generate java code using bnfc and
    -- make sure that the generated skeleton compiles with javac.
    , makeShellyTest "compile VisitSkel.java" $
        withTmpDir $ \tmp -> do
            cp grammar tmp
            cd tmp
            cmd "bnfc" "--java" (filename grammar)
            let visitClass = lang </> "VisitSkel.java"
            assertFileExists visitClass
            cmd "javac" visitClass
    ]
  where
    lang = toTextArg (basename grammar)
    name = pathToString (filename grammar)

-- | Issue #31
issue31 :: Test
issue31 = makeShellyTest "#31 Problem with multiples `rules` declaration with a common prefix" $
    withTmpDir $ \tmp -> do
        cp "regression-tests/31_multirules.cf" (tmp </> "grammar.cf")
        cd tmp
        cmd "bnfc" "--java" "grammar.cf"
