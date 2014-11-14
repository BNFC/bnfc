module HaskellTests (all) where

import Data.Monoid ((<>))
import TestUtils
import Shelly
import Prelude hiding (FilePath, all)
import Filesystem.Path (filename, basename)
import TestData (exampleGrammars)

all = makeTestSuite "Haskell backend" [testSkels]

-- | Given the path to a grammar file, this will generate haskell code using bnfc
-- and make sure that the generated skeleton compiles with ghc.
testSkel :: FilePath -> Test
testSkel grammar = makeShellyTest (pathToString (filename grammar)) $
    withTmpDir $ \tmp -> do
        cp grammar tmp
        cd tmp
        cmd "bnfc" "--haskell" (filename grammar)
        assertFileExists skelf
        cmd "ghc" skelf
  where lang = toTextArg (basename grammar)
        skelf = ("Skel" <> lang) <.> "hs"

-- | Instanciate the test above with all test grammars
testSkels = makeTestSuite "Compile Skeleton.c" $
    map (testSkel . fst) exampleGrammars
