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

javaBackend :: Backend
javaBackend bnfcBin cfFile testFile =
  shelly $ print_commands True $ withTmpDir $ \temp -> do
    cd temp
    -- Preconditions: testing for the existence of the input files
    assertExists cfFile
    assertExists testFile
    -- TODO test existance and executability of bnf
    bnfc "-m" "-java" cfFile
    make
    -- Now we run the test programme, passing the content of
    -- the test source file on stdin
    -- Note that we don't print stdout because it can be very verbose
    -- and we only rely on the status code returned by the test
    -- program to decide if the test passes or fails
    readfile testFile >>= setStdin
    java testProg
  where make = cmd "make"
        bnfc = cmd bnfcBin
        java = cmd "java"
        testProg = basename cfFile </> "Test"

-- Main is defined in SystemTesting, we just pass our backend as a parameter
main = systemTestMain javaBackend

