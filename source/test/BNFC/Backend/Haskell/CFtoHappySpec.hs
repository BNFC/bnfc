module BNFC.Backend.Haskell.CFtoHappySpec where

import Test.Hspec
import Text.PrettyPrint (render)

import BNFC.Backend.Haskell.CFtoHappy

rendersTo a b = render a `shouldBe` b

spec = do
  describe "convert" $ do
    it "quotes backslashes" $
      convert "\\" `rendersTo` "'\\\\'"

    it "quotes backslashes as part of a longer string" $
      convert "/\\" `rendersTo` "'/\\\\'"
