module HaskellCnfTests (all) where

import Prelude hiding (all)
-- import Shelly

import ParameterizedTests hiding (all)
import TestUtils

cnf :: TestParameters
cnf = haskellParameters
  { tpName        = "Haskell/CNF"
  , tpBnfcOptions = ["--haskell", "--cnf"]
  , tpBuild       = tpMake
  }

-- cnfOld :: TestParameters
-- cnfOld = TP
--   { tpName = "Haskell/CNF"
--   , tpBnfcOptions = ["--haskell", "--cnf"]
--   , tpBuild = tpMake
--   , tpRunTestProg =
--          (\ _lang args -> do
--             bin <- canonicalize $ "." </> "TestCNF"
--               -- This naive approach does not work: ("TestCNF" <> lang)
--               -- Because the Haskell backend does some manipulation of names
--               -- to conform with Haskell module name syntax.
--             cmd bin args
--          )
--   }

-- The CNF backend does not exactly work like other backend and some tests are
-- not applicable as-is. We create a restricted test suite just for it.
all :: Test
all = makeTestSuite "Haskell/CNF"
    [ exampleTests cnf, exitCodeTest cnf ]
