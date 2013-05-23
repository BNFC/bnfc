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
        not( isUsageError mode) ==> parseMode (words (show mode)) `shouldBe` mode
    it "returns Help on an empty list of arguments" $
      parseMode [] `shouldBe` Help
    it "returns Help if given --help" $
      parseMode ["--help"] `shouldBe` Help
    it "returns Version if given --version" $
      parseMode ["--version"] `shouldBe` Version
    it "prioritize --help over --version, no mater the order" $
      (parseMode ["--version", "--help"], parseMode ["--help", "--version"])
        `shouldBe` (Help, Help)
    it "returns an error if help is given an argument" $
      isUsageError (parseMode ["--help=2"]) `shouldBe` True
    it "returns an error if the grammar file is missing" $
      parseMode["--haskell"] `shouldBe` UsageError "Missing grammar file"
    it "returns an error if multiple grammar files are given" $
      parseMode["--haskell", "file1.cf", "file2.cf"]
        `shouldBe` UsageError "only one grammar file is allowed"
    it "returns an error if multiple target languages are given" $
      parseMode["--haskell", "--c", "file.cf"]
        `shouldBe` UsageError "only one target language is allowed"
    it "accept latex as a target language" $
      parseMode["--latex", "--makefile", "file.cf"]
        `shouldBe` Target TargetLatex ["--makefile"] "file.cf"

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
-- Note: we only use letters x,y,z to make (almost) sure that we are not
-- going to generate accidentally an global/target language option
arbitraryOption :: Gen String
arbitraryOption = oneof [arbitraryShortOption, arbitraryLongOption]
  where arbitraryShortOption = liftM (('-':) . (:[])) (elements ['x'..'z'])
        arbitraryLongOption  = liftM ("--" ++) (stringOf1 ['x'..'z'])

-- Arbitrary instance for Mode
instance Arbitrary Mode where
  arbitrary = oneof
    [ return Help
    , return Version
    , liftM UsageError arbitrary          -- generates a random error message
    , do target <- arbitraryTarget        -- random target
         cfFile <- arbitraryFilePath "cf"
         args <- listOf arbitraryOption
         return $ Target target args cfFile
    ]
