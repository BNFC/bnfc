module Main (main) where

import Test.Framework (htfMain)

import HaskellCnfTests
import ParameterizedTests
import PygmentsTests
import RegressionTests
import OutputParser

main = htfMain $
  -- Use : and [] for this list such that lines can be swapped swiftly
  -- (avoids the usual problems when trying to switch the first line
  -- with a later line).

  -- ParameterizedTests.layoutTest :
  -- ParameterizedTests.current :  -- Uncomment for prioritized test case.
  -- -- RegressionTests.current :
  ParameterizedTests.all :
  RegressionTests.all    :
  ParameterizedTests.layoutTest :
  OutputParser.tests     :
  HaskellCnfTests.all    :
  PygmentsTests.all      :
  []
