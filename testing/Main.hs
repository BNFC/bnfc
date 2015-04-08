module Main (main) where

import Test.Framework (htfMain)

import CTests
import HaskellCnfTests
import ParameterizedTests
import PygmentsTests
import RegressionTests
import OutputParser

main = htfMain [ ParameterizedTests.all
               , RegressionTests.all
               , PygmentsTests.all
               , CTests.all
               , HaskellCnfTests.all
               , OutputParser.tests ]
