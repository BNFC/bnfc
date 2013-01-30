module Main where

import Test.HUnit
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import qualified Common.Makefile as Makefile

main = defaultMain tests

tests =
  [ testGroup "Makefile"
      [ testCase "Simple make rule" test_rule1
      , testCase "Rule without recipe" test_rule2
      , testCase "Documentation rules" test_doc_rule]]

test_rule1 = "main: file1 file2\n\tdo something\n\n"
          @=? Makefile.mkRule "main" ["file1","file2"] ["do something"] ""

test_rule2 = "main: program.exe\n\n"
          @=? Makefile.mkRule "main" ["program.exe"] [] ""

test_doc_rule = "doc: test.ps\n\ntest.ps: test.dvi\n\tdvips test.dvi -o test.ps\n\ntest.dvi: test.tex\n\tlatex test.tex\n\n"
             @=? Makefile.mkDoc "test.tex" ""
