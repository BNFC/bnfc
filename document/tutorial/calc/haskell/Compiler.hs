module Compiler where

import AbsCalc

compile :: Exp -> IO ()
compile x = case x of
  EAdd exp0 exp  -> compile exp0 >> compile exp >> putStrLn "iadd"
  ESub exp0 exp  -> compile exp0 >> compile exp >> putStrLn "isub"
  EMul exp0 exp  -> compile exp0 >> compile exp >> putStrLn "imul"
  EDiv exp0 exp  -> compile exp0 >> compile exp >> putStrLn "idiv"
  EInt n  -> putStrLn ("ldc " ++ show n)
