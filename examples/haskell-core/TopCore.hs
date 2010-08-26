module Main where

import TestCore
import ParCore
import System

main :: IO ()
main = do
  x:_ <- getArgs
  runFile pModule x

