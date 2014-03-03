{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module BNFC.OptionsSpec where

import Test.Hspec
import Test.QuickCheck
import System.Console.GetOpt
import System.FilePath ((<.>))
import BNFC.WarningM
import Control.Monad (liftM, liftM2)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.FilePath (takeBaseName)

import BNFC.Options -- SUT

spec :: Spec
spec = do

  describe "parseMode" $ do

    it "parses random generated modes" $
      forAll arbitrary $ \mode ->
        not(isUsageError mode) ==> parseMode (linearize mode) `shouldBe` mode

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
        `shouldBe` UsageError "Too many arguments"

    it "returns an error if multiple target languages are given" $
      parseMode["--haskell", "--c", "file.cf"]
        `shouldBe` UsageError "only one target language is allowed"

    it "accept latex as a target language" $ do
      parseMode["--latex", "-m", "file.cf"]
        `shouldBe` Target TargetLatex (defaultOptions {make = True, lang = "file"}) "file.cf"

    it "accept 'old style' options" $
      parseMode["-haskell", "-m", "-glr", "file.cf"]
        `shouldBe` Target TargetHaskell (defaultOptions {make = True, lang = "file", glr = GLR}) "file.cf"

  describe "myGetOpt'" $
    it "returns the input arguments if it cannot parse any options" $
      forAll (listOf randomOption) $ \args ->
        myGetOpt' [] args `shouldBe` ([]::[()],args,[])

  describe "OptParse monad" $ do
    it "returns Help if no other mode is selected" $
      forAll (listOf arbitraryOption) $ \args ->
        runOptParse [] (return ()) `shouldBe` Help

    it "returns any declared mode" $
      forAll arbitrary $ \m ->
        runOptParse [] (setmode m) `shouldBe` m

    it "always returns the first declared mode" $
      forAll arbitrary $ \(m1,m2) ->
        runOptParse [] (setmode m1 >> setmode m2) `shouldBe` m1

  describe "Old option translation" $
    it "translate -haskell to --haskell" $
      translateOldOptions ["-haskell"] `shouldBe` ["--haskell"]

-- ~~~ Useful functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Turn a mode in a list of argiments
-- FIXME: only generate the backend flag currently
linearize :: Mode -> [String]
linearize mode = case mode of
    Help            -> [o "help"]
    Version         -> [o "version"]
    Target t _ file -> [lin t, file]
  where lin t = fromJust $ lookup t targets
        targets = map (\(Option _ [s] (NoArg t) _ ) -> (t,o s)) targetOptions
        o = ("--"++)


-- ~~~ Arbitrary instances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

--  import BNFC.Options
--  import Test.QuickCheck
--  import System.FilePath ((<.>))

randomOption :: Gen String
randomOption = oneof [ nonOption, noArg, withArg ]
  where nonOption = stringOf1 ['a'..'z'] -- non-option argument
        noArg     = nonOption >>= return . ("--"++) -- flag
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
    , do target <- arbitrary              -- random target
         cfFile <- arbitraryFilePath "cf"
         let args = defaultOptions { lang = takeBaseName cfFile}
         return $ Target target args cfFile
    ]
