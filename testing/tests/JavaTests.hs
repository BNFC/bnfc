module JavaTests (all) where

import TestUtils
import Shelly
import Prelude hiding (FilePath, all)
import Filesystem.Path (filename, basename)
import TestData (exampleGrammars)

all = makeTestSuite "Java backend" [testVisitorSkels]

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
