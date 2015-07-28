module BNFC.CFSpec where

import Test.Hspec

-- SUT:
import BNFC.CF

spec :: Spec
spec = do
  describe "Show Cat" $ do
    it "shows (Cat \"Def\") as \"Def\"" $
        show (Cat "Def") `shouldBe` "Def"
    it "shows (ListCat (Cat \"Thing\")) as \"[Thing]\"" $
        show (ListCat (Cat "Thing")) `shouldBe` "[Thing]"
    it "shows (CoercCat \"Expr\" 3) as \"Expr3\"" $
        show (CoercCat "Expr" 3) `shouldBe` "Expr3"
    it "shows (ListCat (CoercCat \"Expr\" 2)) as \"[Expr2]\"" $
        show (ListCat (CoercCat "Expr" 2)) `shouldBe` "[Expr2]"
    it "shows (TokenCat \"Abc\") as \"Abc\"" $
        show (TokenCat "Abc") `shouldBe` "Abc"

  describe "strToCat" $ do
    it "reads \"Abc\" to Cat \"Abc\"" $
        strToCat "Abc" `shouldBe` Cat "Abc"
    it "reads \"Abc123\" to CoercCat \"Abc\" 123" $
        strToCat "Abc123" `shouldBe` CoercCat "Abc" 123
    it "reads \"[Expr2]\" to ListCat (CoercCat \"Expr\" 2)" $
        strToCat "[Expr2]" `shouldBe` ListCat (CoercCat "Expr" 2)

  describe "identCat" $ do
    it "returns ListC for (ListCat (Cat \"C\"))" $
        identCat (ListCat (Cat "C")) `shouldBe` "ListC"
    it "returns C3 for (CoercCat \"C\" 3)" $
        identCat (CoercCat "C" 3) `shouldBe` "C3"

  describe "catOfList" $ do
    it "returns Cat \"A\" for (ListCat (Cat \"A\"))" $
        catOfList (ListCat (Cat "A")) `shouldBe` Cat "A"
    it "returns Cat \"B\" for (Cat \"B\")" $
        catOfList (Cat "B") `shouldBe` Cat "B"

  describe "precCat" $ do
    it "returns 0 for a regular category" $ do
        precCat (Cat "Abc") `shouldBe` 0
    it "returns the precedence of a CoercCat" $
        precCat (CoercCat "Abc" 4) `shouldBe` 4
    it "returns the precedence of a CoercCat inside of a ListCat" $
        precCat (ListCat (CoercCat "Abc" 2)) `shouldBe` 2

  describe "sameCat" $ do
    it "considers a category to be the same as itself" $
        sameCat (Cat "Abc") (Cat "Abc") `shouldBe` True
    it "considers Abc3 and Abc5 to be the same" $
        sameCat (CoercCat "Abc" 3) (CoercCat "Abc" 5) `shouldBe` True
    it "considers Abc and Abc4 to be the same" $
        sameCat (Cat "Abc") (CoercCat "Abc" 44) `shouldBe` True
    it "considers Foo and Bar to not be the same" $
        sameCat (Cat "Foo") (Cat "Bar") `shouldBe` False

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

