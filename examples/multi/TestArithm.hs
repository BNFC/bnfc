module Main where

import qualified LexArithm_C
import qualified LexArithm_JVM
import qualified ParArithm_C
import qualified ParArithm_JVM
import qualified PrintArithm_C
import qualified PrintArithm_JVM
import AbsArithm

import ErrM

import System ( getArgs )

main :: IO ()
main = do
  i:o:f:_ <- getArgs
  s <- readFile f
  case parse i s of
    Ok t -> putStrLn $ prin o t
    Bad s -> error s

parse i = case i of
  "C" -> ParArithm_C.pExp . ParArithm_C.myLexer
  "JVM" -> ParArithm_JVM.pExp . ParArithm_JVM.myLexer

prin o = case o of
  "C" -> PrintArithm_C.printTree
  "JVM" -> PrintArithm_JVM.printTree
