module BNFC.Backend.Common.MakefileSpec where

import Test.Hspec

import BNFC.Backend.Base (execBackend)
import BNFC.Options (defaultOptions,make)
import BNFC.Backend.Common.Makefile -- SUT

spec :: Spec
spec = do
  describe "mkRule" $ do

    it "produces makefile rules correctly" $
      mkRule "main" ["file1","file2"] ["do something"] ""
        `shouldBe` "main: file1 file2\n\tdo something\n\n"

    it "produce mafefile rules without receipes" $
      mkRule "main" ["program.exe"] [] ""
       `shouldBe` "main: program.exe\n\n"

  describe "mkVar" $
    it "writes variables" $
      mkVar "FOO" "bar" "" `shouldBe` "FOO=bar\n"


  describe "mkMakefile" $ do
    it "uses the names in the options dictionary" $
      let opts = defaultOptions { make = Just "MyMakefile" } in
      execBackend (mkMakefile opts "") `shouldReturn` [("MyMakefile","")]
