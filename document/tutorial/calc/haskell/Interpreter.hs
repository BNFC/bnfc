module Interpreter where

import AbsCalc

interpret :: Exp -> Integer
interpret x = case x of
  EAdd exp0 exp  -> interpret exp0 + interpret exp
  ESub exp0 exp  -> interpret exp0 - interpret exp
  EMul exp0 exp  -> interpret exp0 * interpret exp
  EDiv exp0 exp  -> interpret exp0 `div` interpret exp
  EInt n  -> n
