module Main where

import Test.HUnit
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import qualified BNFC.Backend.Common.Makefile as Makefile

main = defaultMain tests

tests =
  [ testGroup "Makefile"
      [ testCase "Simple make rule" testRule1
      , testCase "Rule without recipe" testRule2
      , testCase "Documentation rules" testDocRule]]

testRule1 = "main: file1 file2\n\tdo something\n\n"
          @=? Makefile.mkRule "main" ["file1","file2"] ["do something"] ""

testRule2 = "main: program.exe\n\n"
          @=? Makefile.mkRule "main" ["program.exe"] [] ""

testDocRule = "doc: test.pdf\n\ntest.pdf: test.tex\n\tpdflatex test.tex\n\n"
             @=? Makefile.mkDoc "test.tex" ""
