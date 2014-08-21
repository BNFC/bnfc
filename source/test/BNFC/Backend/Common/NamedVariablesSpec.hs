module BNFC.Backend.Common.NamedVariablesSpec where

import Test.Hspec
import Test.QuickCheck

import BNFC.CF (Cat)

import BNFC.Backend.Common.NamedVariables -- SUT

genCat:: Gen Cat
genCat = frequency [(10,simpleCat), (1,listCat)]
  where simpleCat = elements ["Cat1", "Cat2", "Cat3"]
        listCat   = simpleCat >>= \c -> return ("[" ++ c ++ "]")

spec :: Spec
spec = do
  describe "getVars" $ do

    it "returns a list of the same length as the input list" $
      forAll (listOf genCat) $ \l -> length l == length (getVars l)

    it "leaves the name of the (non list) category untouched" $
      forAll (listOf genCat) $ \l ->
        all (notElem '[') l ==> l == map fst (getVars l)

    it "give the output described in the example" $
      getVars ["A", "B", "A"] `shouldBe` [("A", 1), ("B", 0), ("A", 2)]
