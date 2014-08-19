{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module BNFC.OptionsSpec where

import Control.Monad (liftM, liftM2)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.Console.GetOpt
import System.FilePath ((<.>), takeBaseName)
import Test.Hspec
import Test.QuickCheck

import BNFC.Options -- SUT
import BNFC.WarningM


-- Expectation that a particular option has a particular value
shouldSet :: (Eq a, Show a) => Mode -> (SharedOptions -> a, a) -> Expectation
shouldSet (Target opts _) (option, value) = option opts `shouldBe` value

spec :: Spec
spec = do

  describe "parseMode" $ do

    it "returns Help on an empty list of arguments" $
      parseMode [] `shouldBe` Help

    it "returns Help if given --help" $
      parseMode ["--help"] `shouldBe` Help

    it "returns Version if given --version" $
      parseMode ["--version"] `shouldBe` Version

    it "returns an error if help is given an argument" $
      isUsageError (parseMode ["--help=2"]) `shouldBe` True

    it "If no language is specified, it should default to haskell" $
      parseMode["file.cf"] `shouldSet` (target, TargetHaskell)

    it "returns an error if the grammar file is missing" $
      parseMode["--haskell"] `shouldBe` UsageError "Missing grammar file"

    it "returns an error if multiple grammar files are given" $
      parseMode["--haskell", "file1.cf", "file2.cf"]
        `shouldBe` UsageError "Too many arguments"

    it "sets the language name to the basename of the grammar file" $
      parseMode["foo.cf"] `shouldSet` (lang, "foo")

    it "accept 'old style' options" $ do
      parseMode["-haskell", "-m", "-glr", "file.cf"]
        `shouldSet` (target, TargetHaskell)
      parseMode["-haskell", "-m", "-glr", "file.cf"]
        `shouldSet` (make, Just "Makefile")
      parseMode["-haskell", "-m", "-glr", "file.cf"]
        `shouldSet` (glr, GLR)

  it "accept latex as a target language" $
    parseMode["--latex", "file.cf"] `shouldSet` (target, TargetLatex)

  describe "Old option translation" $ do
    it "translate -haskell to --haskell" $
      translateOldOptions ["-haskell"] `shouldBe` ["--haskell"]

    describe "--makefile" $ do

      it "is off by default" $
        parseMode["--c", "foo.cf"] `shouldSet` (make, Nothing)

      it "uses the file name 'Makefile' by default" $
        parseMode["--c", "-m", "foo.cf"] `shouldSet` (make, Just "Makefile")

      context "when using the option with an argument" $
        it "uses the argument as Makefile name" $
          parseMode["--c", "-mMyMakefile", "foo.cf"]
            `shouldSet` (make, Just "MyMakefile")

-- ~~~ Arbitrary instances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

randomOption :: Gen String
randomOption = oneof [ nonOption, noArg, withArg ]
  where nonOption = stringOf1 ['a'..'z'] -- non-option argument
        noArg     = liftM ("--"++) nonOption -- flag
        withArg   = do
          arg   <- nonOption
          flag  <- noArg
          return $ flag ++ "=" ++ arg

-- Helper function that generates a string of random length using the given
-- set of characters. Not that the type signature explicitely uses
-- [Char] and not String for documentation purposes
stringOf :: [Char] -> Gen String
stringOf = listOf . elements

-- | Same as stringOf but only generates non empty strings
stringOf1 :: [Char] -> Gen String
stringOf1 = listOf1 . elements


instance Arbitrary Target where arbitrary = elements [minBound .. ]

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
    , do target' <- arbitrary              -- random target
         cfFile <- arbitraryFilePath "cf"
         let args = defaultOptions { lang = takeBaseName cfFile, target = target'}
         return $ Target args cfFile
    ]
