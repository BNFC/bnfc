{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Data.String.QQ     (s)
import System.Environment (getArgs)
import Test.Framework     (htfMain)

import License
import qualified SucceedLBNFTests
import qualified FailLBNFTests
import qualified ParameterizedTests
import qualified PygmentsTests
import qualified RegressionTests
import qualified OutputParser

main = do
  args <- getArgs
  if | "--license" `elem` args -> greet license
     | "--help"    `elem` args -> greet usage
     | "-h"        `elem` args -> greet usage
     | otherwise -> runAllTests

greet :: String -> IO ()
greet msg = do
  putStrLn "bnfc-system-tests: Runs BNFC system testsuite."
  putStrLn ""
  putStr msg

usage :: String
usage = [s|
Start bnfc-system-tests from inside `testing` directory.

Options:
--license   Print copyright and license text.
--help, -h  Print this help text.
|]

runAllTests = do
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
    PygmentsTests.all      :
    []
