module BNFC.WarningM where

import qualified Control.Monad.Writer as W

-- Monad that allows pure computation to output
-- warnings
type WithWarnings a = W.Writer [String] a

-- Run the computation and return both the value
-- and the warnings
run :: WithWarnings a -> (a,[String])
run = W.runWriter

-- Run the computation and print the warnings
putWarnings :: WithWarnings a -> IO a
putWarnings c = do
  let (v,warnings) = run c
  mapM_ putStrLn warnings
  return v

hasWarnings :: WithWarnings a -> Bool
hasWarnings c = let (_,warnings) = run c in not (null warnings)

-- Output a warning
warn :: String -> WithWarnings ()
warn s = W.tell [s]
