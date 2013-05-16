module BNFC.Backend.Common.MakefileSpec where

import Test.Hspec

import BNFC.Backend.Common.Makefile -- SUT

spec :: Spec
spec = describe "BNFC.Backend.Common.Makefile" $ do
  describe "mkRule" $ do

    it "prduce makefile rules correctly" $
      mkRule "main" ["file1","file2"] ["do something"] ""
        `shouldBe` "main: file1 file2\n\tdo something\n\n"

    it "produce mafefile rules without receipes" $
      mkRule "main" ["program.exe"] [] ""
        `shouldBe` "main: program.exe\n\n"
