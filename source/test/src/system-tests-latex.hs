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
import SystemTesting
import Filesystem.Path

default (Text)

haskellBackend :: Backend
haskellBackend bnfc cfFile testFile =
  shelly $ print_commands True $ withTmpDir $ \temp -> do
    cd temp
    -- Preconditions: testing for the existence of the input files
    assertExists cfFile
    -- run bnfc on the cf file
    cmd bnfc "--latex" "--makefile" cfFile
    -- Here we only test that the latex file is produced
    assertExists texfile
    assertExists "Makefile"
    -- and that it is accepted by pdflatex
    cmd "make"
    assertExists pdffile
    cmd "make" "clean"
    assertExists texfile
    assertExists "Makefile"
    cmd "make" "cleanall"
    pwd >>= assertEmpty
  where texfile = replaceExtension (basename cfFile) "tex"
        pdffile = replaceExtension texfile "pdf"

-- Main is defined in SystemTesting, we just pass our backend as a parameter
main = systemTestMain haskellBackend
