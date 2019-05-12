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

  -- ParameterizedTests.current :  -- Uncomment for prioritized test case.
  ParameterizedTests.layoutTest :
  ParameterizedTests.all :
  OutputParser.tests     :
  RegressionTests.all    :
  HaskellCnfTests.all    :
  PygmentsTests.all      :
  []
