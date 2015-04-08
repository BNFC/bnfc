module TestUtils
    ( makeShellyTest
    , makeTestSuite
    , makeUnitTest
    , assertFileExists, assertEqual, assertFailure, assertExitCode
    , assertEqualPretty
    , pathToString
    , findFileRegex
    , findFile
    , Test(..)
    , matchFilePath ) where

-- base
import Control.Exception (handle, throwIO, SomeException)
import Control.Monad
import Data.Maybe (listToMaybe)
import Prelude hiding (FilePath)
import Text.Regex.Posix

-- text
import qualified Data.Text as T

-- system-filepath
import Filesystem.Path (filename, stripPrefix)
import Filesystem.Path.CurrentOS (encodeString, toText, encode)

-- shelly
import Shelly

-- htf
import Test.Framework (assertEqualPretty_, assertEqual_)
import Test.Framework.Location (unknownLocation)
import Test.Framework.Pretty (Pretty(..), text, vcat)
import qualified Test.Framework.TestManager as HTF
import Test.Framework.TestTypes

-- hunit
import qualified Test.HUnit as HUnit

-- | Replate the makeTestSuite function from HTF. This one returns a Test
-- object instead of a TestSuite which makes it easier to mix single test
-- and test suites in ohter test suites
makeTestSuite :: TestID -> [Test] -> Test
makeTestSuite id = HTF.testSuiteAsTest . HTF.makeTestSuite id

-- | Replace the makeUnitTest functon from HTF with one that doesn't requite a
-- location.
makeUnitTest :: TestID -> IO () -> Test
makeUnitTest id = HTF.makeUnitTest id unknownLocation

-- Lift HTF's version of assertEqual in MonadIO
assertEqual a b = liftIO $ assertEqual_ unknownLocation a b
assertEqualPretty a b = liftIO $ assertEqualPretty_ unknownLocation a b

-- | Pretty instance for Text (to use with assertEquals)
instance Pretty T.Text where
  pretty = vcat . map text . lines . T.unpack

-- Shortcut function to create a (black box) test from a shelly script
makeShellyTest :: TestID -> Sh () -> Test
makeShellyTest label =
    HTF.makeBlackBoxTest label . handle fixException . shelly . silently
  where
    fixException (ReThrownException x _) = throwIO (x::SomeException)

-- A (Shelly) assertion to check the existense of a file
assertFileExists :: FilePath -> Sh ()
assertFileExists p = test_f p >>= liftIO . HUnit.assertBool errorMessage
  where errorMessage = "Can't find file " ++ encodeString p

-- | Lift HUnit's assertFailure
assertFailure :: String -> Sh ()
assertFailure = liftIO . HUnit.assertFailure

-- | Expect a particular exit code:
assertExitCode :: Int -> Sh () -> Sh ()
assertExitCode c sh = errExit False sh >> lastExitCode >>= assertEqual c

-- | A PrintfArg instance of FilePath to use filepaths in strings (e.g. names
-- of tests). Allows you to do things like:
-- printf "testing %s" (path :: FilePath)
--
-- !! Commented for now as it is only possible with ghc-7.8.3
-- instance Text.Printf.PrintfArg FilePath where
--   formatArg = formatArg . either T.unpack T.unpack . Filesystem.Path.CurrentOS.toText

-- | Convert a FilePath to a string
pathToString = either T.unpack T.unpack . toText

-- | Find a file given a regular expression.
-- Will fail if there is not exactly one file matching
findFileRegex :: String -> Sh FilePath
findFileRegex r = do
    fs <- findWhen (return . matchFilePath r) "." >>= filterM test_f
    when (length fs < 1) $ assertFailure "File not found"
    when (length fs > 1) $ assertFailure $
        "Too many files for regex " ++ r ++ " " ++ show fs
    return (head fs)

-- Find a file given its exact name
findFile n = do
    f <- findWhen (return . (n==) . filename) "."
    case listToMaybe f >>= stripPrefix "." of
        Just f -> return f
        Nothing -> assertFailure "File not found" >> undefined

matchFilePath :: String -> FilePath -> Bool
matchFilePath regex name = encode name =~ regex
