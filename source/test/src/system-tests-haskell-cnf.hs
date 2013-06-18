{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Filesystem.Path (basename)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Text.Printf (printf)
import Shelly
import Prelude hiding (FilePath)
import Data.Text.Lazy (Text)
import Control.Exception (assert)
import Paths_BNFC
import SystemTesting (systemTestMain, Backend, assertExists)

default (Text)

haskellBackendCNF::Backend
haskellBackendCNF bnfcBin cfFile testFile =
  shelly $ print_commands True $ withTmpDir $ \temp -> do
    cd temp
    
    assertExists cfFile
    assertExists testFile
    bnfc "-m" "--cnf" "--haskell" cfFile
    make
    readfile testFile >>= setStdin
    test
  where make = cmd "make"
        bnfc = cmd bnfcBin
        testProg = "TestCNF"
        test = cmd ("." </> testProg)

-- Main is defined in SystemTesting, we just pass our backend as a parameter
main = do
  systemTestMain haskellBackendCNF
