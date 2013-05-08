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

haskellBackend :: Backend
haskellBackend bnfcBin cfFile testFile =
  shelly $ print_commands True $ withTmpDir $ \temp -> do
    cd temp
    -- Preconditions: testing for the existence of the input files
    assertExists cfFile
    assertExists testFile
    -- run bnfc on the cf file
    bnfc cfFile
    -- Here we only test that the latex file is produced
    assertExists latexFile
    -- and that it is accepted by pdflatex
    pdflatex latexFile
  where pdflatex = cmd "pdflatex"
        latexFile = decodeString ("Doc" ++ (encodeString $ basename cfFile)) <.> "tex"
        bnfc = cmd bnfcBin

-- Main is defined in SystemTesting, we just pass our backend as a parameter
main = systemTestMain haskellBackend
