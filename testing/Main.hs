module Main (main) where

import Test.Framework (htfMain)

import qualified SucceedLBNFTests
import qualified FailLBNFTests
import qualified HaskellCnfTests
import qualified ParameterizedTests
import qualified PygmentsTests
import qualified RegressionTests
import qualified OutputParser

main = do
  succeedLBNFTests <- SucceedLBNFTests.all
  failLBNFTests    <- FailLBNFTests.all
  htfMain $
    -- Use : and [] for this list such that lines can be swapped swiftly
    -- (avoids the usual problems when trying to switch the first line
    -- with a later line).

    succeedLBNFTests :
    failLBNFTests :
    -- ParameterizedTests.layoutTest :
    ParameterizedTests.current :  -- Uncomment for prioritized test case.
    -- RegressionTests.current :
    ParameterizedTests.all :
    RegressionTests.all    :
    ParameterizedTests.layoutTest :
    OutputParser.tests     :
    HaskellCnfTests.all    :
    PygmentsTests.all      :
    []
