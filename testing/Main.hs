module Main (main) where

import Test.Framework (htfMain)

import HaskellCnfTests
import ParameterizedTests
import PygmentsTests
import RegressionTests
import OutputParser

main = htfMain
  [ RegressionTests.all
  , ParameterizedTests.all
  , PygmentsTests.all
  , HaskellCnfTests.all
  , OutputParser.tests
  ]
