module BNFC.GetCFSpec where

import Test.Hspec

-- SUT:
import BNFC.GetCF

import BNFC.CF
import qualified AbsBNF as Abs

spec :: Spec
spec = do
  describe "transItem" $ do

    it "translate a non-terminal" $
        transItem (Abs.NTerminal (Abs.IdCat (Abs.Ident "Foo3")))
            `shouldBe` [Left (CoercCat "Foo" 3)]

    it "translate a terminal" $
        transItem (Abs.Terminal "foobar") `shouldBe` [Right "foobar"]

    it "skips empty terminals" $
        transItem (Abs.Terminal "") `shouldBe` []

    it "splits multiwords terminals" $
        transItem (Abs.Terminal "foo bar") `shouldBe` [Right "foo", Right "bar"]

  describe "checkRule" $ do

    it "returns an error if the rule uses an unknown category" $ do
        let rulep = Rule ("Foo", ("Foo", [])) (Cat "Bar") [Left (Cat "Baz")]
            rule = Rule "Foo" (Cat "Bar") [Left (Cat "Baz")]
            cf = CFG [] [] [] [] [] [rule]
            expected =
                "no production for Baz, appearing in rule\n    Foo. Bar ::= Baz"
        checkRule cf rulep `shouldBe` Just expected
