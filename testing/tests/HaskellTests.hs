module HaskellTests (all) where

import Control.Monad (forM_)
import TestUtils
import Shelly
import Data.Text (Text)
import Prelude hiding (FilePath, all)
import Filesystem.Path (filename)
import TestData

all = makeTestSuite "Haskell"
    [ makeTestSuite "default options"
        (map (factory []) exampleGrammars)
    , makeTestSuite "with functor"
        (map (factory ["--functor"]) exampleGrammars)
    , makeTestSuite "with namespace"
        (map (factory ["-p", "Language", "-d"]) exampleGrammars)
    , issue128
    ]


-- | Test factory
-- Given a list of extra options, a grammar files and eventually some example
-- programma parsable with the grammar, this produce a testsuite where code is
-- generated using bnfc and then either
-- - uses 'make' to build the test program
-- - uses ghc to build the skeleton file
factory :: [Text] -> Example -> Test
factory options (grammar,examples) = makeTestSuite name
    [ makeShellyTest "test program" $
        withTmpDir $ \tmp -> do
            cp grammar tmp
            forM_ examples $ flip cp tmp
            cd tmp
            bnfc ["-m", toTextArg (filename grammar)]
            cmd "make"
            -- cmd "tree" "."
            testProgram <- findFileRegex "Test\\w*$"
            forM_ examples $ \example -> do
                readfile (filename example) >>= setStdin
                cmd testProgram
    -- | Given the grammar file, this will generate haskell code using bnfc and
    -- make sure that the generated skeleton compiles with ghc.
    , makeShellyTest "compile SkelXxx.hs" $
        withTmpDir $ \tmp -> do
            cp grammar tmp
            cd tmp
            bnfc [toTextArg (filename grammar)]
            skelf <- findFileRegex "Skel.*\\.hs$"
            assertFileExists skelf
            cmd "ghc" skelf
    ]
  where
    name = pathToString (filename grammar)
    bnfc = command_ "bnfc" ("--haskell" : options)

-- | Issue #128
issue128 :: Test
issue128 = makeShellyTest "Cannot use B as a constructor in haskell #128" $
    withTmpDir $ \tmp -> do
        cp "regression-tests/128_bar.cf" (tmp </> "grammar.cf")
        cd tmp
        cmd "bnfc" "--haskell" "-m" "grammar.cf"
        cmd "make"
