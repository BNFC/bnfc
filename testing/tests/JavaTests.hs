module JavaTests (all) where

import TestUtils
import Shelly
import Prelude hiding (FilePath, all)
import Filesystem.Path (filename, basename)
import TestData (exampleGrammars)

all = makeTestSuite "Java backend" [testVisitorSkels, issue31]

-- | Given the path to a grammar file, this will generate java code using bnfc
-- and make sure that the generated Visitor skeleton compiles with javac.
testVisitorSkel :: FilePath -> Test
testVisitorSkel grammar = makeShellyTest (pathToString (basename grammar)) $
    withTmpDir $ \tmp -> do
        cp grammar tmp
        cd tmp
        cmd "bnfc" "--java" (filename grammar)
        let visitClass = language </> "VisitSkel.java"
        assertFileExists visitClass
        cmd "javac" visitClass
  where language = basename grammar

-- | Instanciate the test above with all test grammars
testVisitorSkels = makeTestSuite "Compile VisitSkel.java" $
    map (testVisitorSkel . fst) exampleGrammars

-- | Issue
issue31 :: Test
issue31 = makeShellyTest "#31 Problem with multiples `rules` declaration with a common prefix" $
    withTmpDir $ \tmp -> do
        cp "regression-tests/31_multirules.cf" (tmp </> "grammar.cf")
        cd tmp
        cmd "bnfc" "--java" "grammar.cf"
