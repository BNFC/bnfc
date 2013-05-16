module BNFC.OptionsSpec where

import Test.Hspec
import Test.QuickCheck
import System.FilePath ((<.>))
import BNFC.WarningM
import Control.Monad (liftM2)

import BNFC.Options -- SUT

spec :: Spec
spec = describe "BNFC.Options" $ do

  describe "isCfFile" $ do

    it "returns True for any file name ending with one of the allowed extensions" $
      let filenames = do
            liftM2 (<.>) (listOf1 $ elements ['a'..'z']) (elements allowed_exts)
      in forAll filenames isCfFile

  describe "translateArguments" $ do
    it "has warnings iff one of the arguments is deprecated" $
      let genArguments = listOf $ elements (deprecated ++ others)
          deprecated =  [ "-java","-java1.5","-java1.4","-c","-cpp","-cpp_stl"
                        , "-cpp_no_stl","-csharp","-ocaml","-haskell"
                        , "-prof","-gadt","-alex1","-alex1","-alex3"
                        , "-sharestrings","-bytestrings","-glr","-xml","-xmlt"
                        , "-vs","-wcf" ]
          others =  [ "--java","--java5","--java4","--c","--cpp","--stl"
                    , "--no-stl","--csharp","--ocaml","--haskell"
                    , "--prof","--gadt","--alex1","--alex2","--alex3"
                    , "--sharestrings","--bytestrings","--glr","--xml","--xmlt"
                    , "--vs","--wcf", "-d", "-p", "-l" ]
      in
      forAll genArguments $ \args ->
        any (`elem` deprecated) args == hasWarnings (translateArguments args)

  describe "lookForDeprecatedOptions" $ do

    it "returns nothing on the empty list" $
      lookForDeprecatedOptions [] `shouldBe` []

    it "returns an error message if the arguments contain '--numeric-version'" $
      lookForDeprecatedOptions ["--numeric-version"]
        `shouldBe` ["--numeric-version is deprecated, use --version instead\n"]

    it "returns an error message if the arguments contain '-multi'" $
      lookForDeprecatedOptions ["-multi"]
        `shouldBe` ["-multi is deprecated, use --multilingual instead\n"]

