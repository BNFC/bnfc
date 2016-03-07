module Main (main) where

import Test.Framework (htfMain)

import HaskellCabalTests
import HaskellCnfTests
import ParameterizedTests
import PygmentsTests
import RegressionTests
import OutputParser

main = htfMain [ ParameterizedTests.all
               , RegressionTests.all
               , PygmentsTests.all
               , HaskellCnfTests.all
               , OutputParser.tests
               , HaskellCabalTests.all
               ]
