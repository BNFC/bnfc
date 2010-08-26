module Main where

import LexCalc
import ParCalc
import AbsCalc
import Interpreter

import ErrM

main = do
  interact calc
  putStrLn ""

calc s = 
  let Ok e = pExp (myLexer s) 
  in show (interpret e)
