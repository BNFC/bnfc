module HaskellCnfTests (all) where

import Prelude hiding (all)
import Shelly

import ParameterizedTests hiding (all)
import TestUtils

cnf :: TestParameters
cnf = TP "Haskell/CNF" ["--haskell", "--cnf", "-m"]
         (cmd "make")
         (\_lang args -> do
            bin <- canonicalize ("." </> "TestCNF")
            cmd bin args
         )

-- The CNF backend does not exactly work like other backend and some tests are
-- not applicable as-is. We create a restricted test suite just for it.
all :: Test
all = makeTestSuite "Haskell/CNF"
    [ exampleTests cnf, exitCodeTest cnf ]
