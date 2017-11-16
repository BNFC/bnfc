module Main where

import LexCalc
import ParCalc
import AbsCalc
import Compiler

import System
import ErrM

main = do
  file:_ <- getArgs
  s      <- readFile file
  let Ok e = pExp (myLexer s)
  compile e
