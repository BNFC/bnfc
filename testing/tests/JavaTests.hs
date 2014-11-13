module JavaTests (all) where

import TestUtils
import Shelly
import Prelude hiding (FilePath, all)
import Filesystem.Path (filename, basename)

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
testVisitorSkels =
    makeTestSuite "Compile VisitSkel.java" (map (testVisitorSkel . fst) testData)


-- The data to test the different backends with. The first file should be
-- a lbnf grammar and the list contains example programs written in this
-- languague. The list can contain zero, one or more example files. If there
-- is zero, we only test that the grammar is correctly compiled. If there is
-- ore or more, they are fed to the test program and we expect that it exits
-- successfully (i.e. exit code 0).
testData :: [(FilePath, [FilePath])]
testData =
  [ ( examples</>"cpp"</>"cpp.cf"
    , [ examples</>"cpp"</>"example.cpp"] )

  , ( examples</>"GF"</>"gf.cf"
    , [ examples</>"GF"</>"example.gf"] )

  , ( examples</>"OCL"</>"OCL.cf"
    , [ examples</>"OCL"</>"example.ocl"] )

  , ( examples</>"prolog"</>"Prolog.cf"
    , [ examples</>"prolog"</>"small.pl"
      , examples</>"prolog"</>"simpsons.pl" ] )

  , ( examples</>"C"</>"C.cf"
    , [ examples</>"C"</>"runtime.c"
      , examples</>"C"</>"koe2.c" ] )

  , ( examples</>"C"</>"C4.cf"
    , [ examples</>"C"</>"koe2.c"])

  , ( examples</>"C"</>"C_with_delimiters.cf"
    , [ examples</>"C"</>"small.c" ] )
      -- , examples</>"C"</>"core.c" ] ) -- Fail with CNF!!!

  , ( examples</>"Javalette"</>"JavaletteLight.cf"
    , [examples</>"Javalette"</>"koe.jll"])

  , ( examples</>"LBNF"</>"LBNF.cf"
    , [examples</>"LBNF"</>"LBNF.cf"])

  -- , ( examples</>"Java"</>"java.cf", [] ) -- Cannot be used for testing as
  -- it has duplicate names

  , ( examples</>"Calc.cf", [] )
  , ( examples</>"fstStudio.cf", [] )
  ]
  where examples = ".."</>"examples"
