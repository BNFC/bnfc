module PygmentsTests where

import TestUtils
import Shelly

all = makeTestSuite "Pygments backend"
    [ testPygmentsCalc ]

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
            cmd "virtualenv" "env"
            cmd "env/bin/python" "setup.py" "install"
            assertFileExists "env/bin/pygmentize"
            expected <- readfile "pygments-calc-output.txt"
            readfile "pygments-calc-input.txt" >>= setStdin
            output <- cmd "env/bin/pygmentize" "-l" "calc" "-fraw"
            assertEqual expected output

