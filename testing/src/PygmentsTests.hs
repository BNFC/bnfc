module PygmentsTests where

import TestUtils
import Shelly

all = makeTestSuite "Pygments backend"
    [ testPygmentsCalc
    , testPygmentsSpecialSymbol
    ]

-- TODO: Extract common code as utils
-- FIXME: Choose a better location for test data

testPygmentsCalc :: Test
testPygmentsCalc =
    makeShellyTest "Pygments Backend" $
        withTmpDir $ \tmp -> do
            cp "../examples/Calc.cf" tmp
            cp "data/pygments-calc-input.txt" tmp
            cp "data/pygments-calc-output.txt" tmp
            cd tmp
            cmd "bnfc" "--pygments" "Calc.cf"
            assertFileExists "setup.py"
            assertFileExists "calc/__init__.py"
            cmd "python3" "-m" "venv" "env"
            env <- canonicalize "./env/"
            cmd (env </> "bin/python") "setup.py" "install"
            assertFileExists (env </> "bin/pygmentize")
            expected <- readfile "pygments-calc-output.txt"
            readfile "pygments-calc-input.txt" >>= setStdin
            output <- cmd (env </> "bin/pygmentize") "-l" "calc" "-fraw"
            assertEqual expected output

testPygmentsSpecialSymbol :: Test
testPygmentsSpecialSymbol =
  makeShellyTest "#477 Pygments backend is generating regex with unescaped symbol (^ and $)" $
    withTmpDir $ \tmp -> do
      cp "regression-tests/477_PygmentsSpecialSymbols/Symbols.cf" tmp
      cp "regression-tests/477_PygmentsSpecialSymbols/pygments-symbols-input.txt" tmp
      cp "regression-tests/477_PygmentsSpecialSymbols/pygments-symbols-output.txt" tmp
      cd tmp
      cmd "bnfc" "--pygments" "Symbols.cf"
      assertFileExists "setup.py"
      assertFileExists "symbols/__init__.py"
      cmd "python3" "-m" "venv" "env"
      env <- canonicalize "./env/"
      cmd (env </> "bin/python") "setup.py" "install"
      assertFileExists (env </> "bin/pygmentize")
      expected <- readfile "pygments-symbols-output.txt"
      readfile "pygments-symbols-input.txt" >>= setStdin
      output <- cmd (env </> "bin/pygmentize") "-l" "symbols" "-fraw"
      assertEqual expected output
