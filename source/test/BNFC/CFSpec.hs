module BNFC.CFSpec where

import Test.Hspec

-- SUT:
import BNFC.CF

spec :: Spec
spec = do
  describe "getSeparatorByPrecedence" $ do
    let c0 = CoercCat "C" 0
        c1 = CoercCat "C" 1
        rule0 = Rule "(:)" (ListCat c0) [Left c0, Right ",", Left (ListCat c0)]
        rule1 = Rule "(:)" (ListCat c1) [Left c1, Right ";", Left (ListCat c1)]

    it "returns a single value for a simple list" $
      getSeparatorByPrecedence [rule0] `shouldBe` [(0,",")]

    it "returns as many separators as there are list constructors" $
      getSeparatorByPrecedence [rule0, rule1] `shouldBe` [(1,";"),(0,",")]

    it "ignores additional rules with the same precedence" $
      getSeparatorByPrecedence [rule0, rule1, rule0] `shouldBe` [(1,";"),(0,",")]

