module TestUtils
    ( makeShellyTest, assertFileExists, assertEqual
    , makeTestSuite
    , Test(..) ) where

import Control.Exception (handle, throwIO, SomeException)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS (encodeString)
import Prelude hiding (FilePath)
import Shelly
import Test.Framework (assertEqualPretty_)
import Test.Framework.Location (unknownLocation)
import Test.Framework.Pretty (Pretty(..), text)
import qualified Test.Framework.TestManager as HTF
import Test.Framework.TestTypes
import qualified Test.HUnit as HUnit


-- | Replate the makeTestSuite function from HTF. This one returns a Test
-- object instead of a TestSuite which makes it easier to mix single test
-- and test suites in ohter test suites
makeTestSuite :: TestID -> [Test] -> Test
makeTestSuite id = HTF.testSuiteAsTest . HTF.makeTestSuite id

-- Lift HTF's version of assertEqual in Sh
assertEqual :: (Eq a, Pretty a) => a -> a -> Sh ()
assertEqual a b = liftIO $ assertEqualPretty_ unknownLocation a b

-- | Pretty instance for Text (to use with assertEquals)
instance Pretty T.Text where
  pretty = text . T.unpack

-- Shortcut function to create a (black box) test from a shelly script
makeShellyTest :: TestID -> Sh () -> Test
makeShellyTest label =
    HTF.makeBlackBoxTest label . handle fixException . shelly
  where
    fixException (ReThrownException x _) = throwIO (x::SomeException)

-- A (Shelly) assertion to check the existense of a file
assertFileExists :: FilePath -> Sh ()
assertFileExists p = test_f p >>= liftIO . HUnit.assertBool errorMessage
  where errorMessage = "Can't find file " ++ encodeString p
