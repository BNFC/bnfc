module BNFC.Backend.Common.StrUtilsSpec where

import Test.Hspec

import BNFC.Backend.Common.StrUtils

spec = do
  describe "escapeChars" $ do
    it "escapes \\" $
      escapeChars "\\" `shouldBe` "\\\\"
    it "escapes \"" $
      escapeChars "\"" `shouldBe` "\\\""
    it "escapes '" $
      escapeChars "'" `shouldBe` "\\'"

