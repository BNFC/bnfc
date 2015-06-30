module BNFC.Backend.Common.MakefileSpec where

import Test.Hspec

import BNFC.Backend.Base (execBackend)
import BNFC.Options (defaultOptions,make)
import BNFC.Backend.Common.Makefile -- SUT

spec :: Spec
spec = do
  describe "mkMakefile" $ do
    it "uses the names in the options dictionary" $
      let opts = defaultOptions { make = Just "MyMakefile" } in
      execBackend (mkMakefile opts "") `shouldReturn` [("MyMakefile","")]
