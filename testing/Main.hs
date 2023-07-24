{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Data.String.QQ     (s)
import System.Environment (getArgs)
import Test.Framework     (htfMainWithArgs)

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
     | otherwise -> runAllTests args  -- All other args will pass into HTF

greet :: String -> IO ()
greet msg = do
  putStrLn "bnfc-system-tests: Runs BNFC system testsuite."
  putStrLn ""
  putStr msg

usage :: String
usage = [s|
Start bnfc-system-tests from inside `testing` directory.

Options:
  -n PATTERN  --not=PATTERN               Tests to exclude.
  -l          --list                      List all matching tests.
              --fail-fast                 Fail and abort test run as soon as the first test fails.
              --license                   Print copyright and license text.
  -h          --help                      Print this help text.
|]

runAllTests :: [String] -> IO ()
runAllTests args = do
  succeedLBNFTests <- SucceedLBNFTests.all
  failLBNFTests    <- FailLBNFTests.all
  htfMainWithArgs
    args
    -- Use : and [] for this list such that lines can be swapped swiftly
    -- (avoids the usual problems when trying to switch the first line
    -- with a later line).
    $
    succeedLBNFTests :
    failLBNFTests :
    -- ParameterizedTests.layoutTest :
    -- ParameterizedTests.current :  -- Uncomment for prioritized test case.
    -- RegressionTests.current :
    ParameterizedTests.all :
    RegressionTests.all    :
    ParameterizedTests.layoutTest :
    OutputParser.tests     :
    PygmentsTests.all      :
    []
