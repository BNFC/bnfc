module BNFC.OptionsSpec where

import Test.Hspec
import Test.QuickCheck
import System.FilePath ((<.>))
import BNFC.WarningM
import Control.Monad (liftM, liftM2)
import Data.List (intercalate)

import BNFC.Options -- SUT

spec :: Spec
spec = describe "BNFC.Options" $ do

  describe "isCfFile" $ do

    it "returns True for any file name ending with one of the allowed extensions" $
      forAll (elements allowed_exts >>= arbitraryFilePath) isCfFile

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

  describe "parseMode" $ do
    it "parses random generated modes" $
      forAll arbitrary $ \mode ->
        not( isUsageError mode) ==>parseMode (words (show mode)) `shouldBe` mode

-- ~~~ Arbitrary instances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

--  import BNFC.Options
--  import Test.QuickCheck
--  import System.FilePath ((<.>))

-- Helper function that generates a string of random length using the given
-- set of characters. Not that the type signature explicitely uses
-- [Char] and not String for documentation purposes
stringOf :: [Char] -> Gen String
stringOf = listOf . elements

-- | Same as stringOf but only generates non empty strings
stringOf1 :: [Char] -> Gen String
stringOf1 = listOf1 . elements

-- | Picks a target at random
arbitraryTarget :: Gen Target
arbitraryTarget = elements [minBound .. ]

-- creates a filepath with the given extension
arbitraryFilePath :: String -> Gen FilePath
arbitraryFilePath ext = do
  path <- listOf1 $ stringOf1 ['a'..'z']
  return $ intercalate "/" path <.> ext

-- Generates unix command line options. Can be in long form (ex: --option)
-- or short form (ex: -o)
arbitraryOption :: Gen String
arbitraryOption = oneof [arbitraryShortOption, arbitraryLongOption]
  where arbitraryShortOption = liftM (('-':) . (:[])) (elements ['a'..'z'])
        arbitraryLongOption  = liftM ("--" ++) (stringOf1 ['a'..'z'])

-- Arbitrary instance for Mode
instance Arbitrary Mode where
  arbitrary = oneof
    [ return Help
    , return Version
    , liftM UsageError arbitrary          -- generates a random error message
    , do target <- arbitraryTarget        -- random target
         cfFile <- arbitraryFilePath "cf"
         args <- listOf arbitraryOption
         return $ Target target (args ++ [cfFile])
    ]
