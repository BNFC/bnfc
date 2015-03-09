module Main (main) where

import Test.Framework (htfMain)

import CTests
import HaskellCnfTests
import ParameterizedTests
import PygmentsTests
import RegressionTests

main = htfMain [ ParameterizedTests.all
               , RegressionTests.all
               , PygmentsTests.all
               , CTests.all
               , HaskellCnfTests.all ]
