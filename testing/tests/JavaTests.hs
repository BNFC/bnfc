module JavaTests (all) where

import Control.Monad (forM_, liftM)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Filesystem.Path (filename, stripPrefix, dropExtension)
import Prelude hiding (FilePath, all)
import Shelly

import TestUtils
import TestData

all = makeTestSuite "Java"
    [ makeTestSuite "default options"
        (map (factory []) exampleGrammars)
    , makeTestSuite "with namespace"
        (map (factory ["-p","my.stuff"]) exampleGrammars)
    , makeTestSuite "with jflex"
        (map (factory ["--jflex"]) exampleGrammars)
    , issue31
    ]

-- | Test factory
-- Given a grammar files and eventually some example programma parsable with
-- the grammar, this produce a testsuite where code is generated using bnfc and
-- then either
-- - uses 'make' to build the test program
-- - uses javac to build the skeleton file
factory :: [Text] -> Example -> Test
factory options (grammar,examples) = makeTestSuite name
    [ makeShellyTest "make test program" $
        withTmpDir $ \tmp -> do
            cp grammar tmp
            forM_ examples $ flip cp tmp
            cd tmp
            bnfc ["-m", toTextArg (filename grammar)]
            cmd "make"
            testClass <- liftM dropExtension $ findFile "Test.class"
            forM_ examples $ \example -> do
                readfile (filename example) >>= setStdin
                cmd "java" testClass
    -- | Given the grammar file, this will generate java code using bnfc and
    -- make sure that the generated skeleton compiles with javac.
    , makeShellyTest "compile VisitSkel.java" $
        withTmpDir $ \tmp -> do
            cp grammar tmp
            cd tmp
            bnfc [toTextArg (filename grammar)]
            visitClass <- findFile "VisitSkel.java"
            cmd "javac" visitClass
    ]
  where
    name = pathToString (filename grammar)
    bnfc = command "bnfc" ("--java" : options)
    findFile n = do
        f <- findWhen (return . (n==) . filename) "."
        case listToMaybe f >>= stripPrefix "." of
          Just f -> return f
          Nothing -> assertFailure "File not found" >> undefined

-- | Issue #31
issue31 :: Test
issue31 = makeShellyTest "#31 Problem with multiples `rules` declaration with a common prefix" $
    withTmpDir $ \tmp -> do
        cp "regression-tests/31_multirules.cf" (tmp </> "grammar.cf")
        cd tmp
        cmd "bnfc" "--java" "grammar.cf"
