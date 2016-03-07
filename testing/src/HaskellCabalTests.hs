module HaskellCabalTests (all) where

import Shelly
import Prelude hiding (all,unlines)
import Data.Text(Text,unlines,unpack)
import TestUtils(Test,makeShellyTest,makeTestSuite,assertFileExists)

all = makeTestSuite "Haskell/Cabal"
  [ mkTest target desc ps
  | (desc,ps) <- params
  , target <- ["--haskell","--haskell-gadt"]
  ]

-- |a list of test description and the respective arguments to bnfc
params :: [(String,[Text])]
params =
  [("Cabal only",[])
  ,("Ghc",["--ghc"])
  ,("Functor",["--functor"])
  ,("Xml",["--xml"])
  ,("Xmlt",["--xmlt"])
  ,("Qualified",["-d"])
  ,("Namespace",["-p","Foobar"])
  ,("Qualified namespace",["-p","Foobar","-d"])
  ]

-- |building the Shelly Test
mkTest :: Text -> String -> [Text] -> Test
mkTest target desc bnfcParams =
  makeShellyTest description $ withTmpDir $ \tmp -> do
    cd tmp
    writefile "Test.cf" $ unlines
      [ "Start. S ::= S \"a\" ;"
      , "End.   S ::= ;" ]
    run_ "bnfc" args
    assertFileExists "Test.cabal"
    cmd "cabal" "configure"
    cmd "cabal" "build"
    where
      args :: [Text]
      args = target:"--cabal":bnfcParams ++ ["Test.cf"]

      description :: String
      description = desc++",Target "++drop 2 (unpack target)
