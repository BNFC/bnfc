module BNFC.WarningMSpec where

import Test.Hspec

import BNFC.WarningM -- SUT

spec :: Spec
spec = do

  describe "run" $ do

    it "returns the result of the computation and all raised warnings" $
      run computationWithWarnings `shouldBe` (3,["Coucou", "Hi"])

  describe "putWarnings" $ do

    it "returns the result of the computation" $ do
      putWarnings computationWithWarnings `shouldReturn` 3

  describe "hasWarnings" $ do

    it "returns True if the computation produces warnings" $
      hasWarnings computationWithWarnings `shouldBe` True

    it "returns False if the computation does not produce any warnings" $
      hasWarnings (return ()) `shouldBe` False

  where computationWithWarnings = warn "Coucou" >> warn "Hi" >> return 3
