module HaskellTests (all) where

import Control.Monad (forM_)
import Data.Monoid ((<>))
import TestUtils
import Shelly
import Data.Text (Text)
import Prelude hiding (FilePath, all)
import Filesystem.Path (filename, basename)
import TestData

all = makeTestSuite "Haskell"
    [ makeTestSuite "default options"
        (map (factory []) exampleGrammars)
    , makeTestSuite "with functor"
        (map (factory ["--functor"]) exampleGrammars) ]


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
            command_ "bnfc" ["-m", "--haskell", toTextArg (filename grammar)] options
            cmd "make"
            forM_ examples $ \example -> do
                readfile (filename example) >>= setStdin
                cmd ("." </> ("Test" <> lang))
    -- | Given the grammar file, this will generate haskell code using bnfc and
    -- make sure that the generated skeleton compiles with ghc.
    , makeShellyTest "compile SkelXxx.hs" $
        withTmpDir $ \tmp -> do
            cp grammar tmp
            cd tmp
            cmd "bnfc" "--haskell" (filename grammar)
            assertFileExists skelf
            cmd "ghc" skelf
    ]
  where
    lang = toTextArg (basename grammar)
    name = pathToString (filename grammar)
    skelf = ("Skel" <> lang) <.> "hs"
