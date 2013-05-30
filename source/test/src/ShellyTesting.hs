{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{- This modules contains helper function ta make it easier to integrate
 - shelly in hspec tests
 -}
module ShellyTesting where

import Shelly
import Test.Hspec
import Control.Monad (liftM, (>=>))
import Prelude hiding (FilePath)
import Data.Monoid (mappend)
import Test.HUnit (assertBool)
import qualified Data.Text.Lazy as LT
import BNFC.Backend.Latex -- SUT
default (LT.Text)

-- | This is a wrapper arround shelly that provides an easy environment
-- for testing. It changes the current dir to a temporary directory where
-- Calc.cf has been copied and adds bnfc's build path (usually dist/build/bnfc)
-- to the PATH environment variable.
shTest :: Sh a -> IO a
shTest cmds = do
  shelly $ silently $ withTmpDir $ \temp -> do
  -- We assume that the tests are run from the directory where the .cabal
  -- file is and that it is still the current working dir so we can compute
  -- the path to the needed files
  -- 1. Add bnfc's build dir to the current path
  absPath ( "dist"</>"build"</>"bnfc") >>= prependToPath
  -- cmd "export" ("PATH=" `mappend` bnfcPath `mappend` ":$PATH")
  -- 2. now we copy the Calc grammar to the temporary dir
  calc <- absPath (".." </> "examples" </> "Calc.cf")
  cp calc temp
  -- 3. Finally we move to the tmp dir and run the commands
  -- passed as an argument
  cd temp
  cmds

-- | Because of a bug in shelly, we cannot simply call bnfc
-- with `cmd "bnfc"`. Instead we have to use this `env` trick.
-- This is a shortcut that does exactly that.
bnfc :: [LT.Text] -> Sh LT.Text
bnfc args = run "env" ("bnfc":args)

-- | A custom expectation that runs a Shelly script and check afterwards that
-- the specified files are present in the temporary directory
shouldCreate :: Sh a -> [FilePath] -> Expectation
shouldCreate command fpaths = shTest $ command >> mapM_ assertExists_sh fpaths
  where assertBool_sh msg b = liftIO $ assertBool msg b
        assertExists_sh fp
          = test_e fp >>= assertBool_sh ("File " ++ show fp ++ " wasn't created")

-- | Same as appendToPath but put the new path first
prependToPath :: FilePath -> Sh ()
prependToPath = absPath >=> \filepath -> do
  tp <- toTextWarn filepath
  pe <- get_env_text path_env
  setenv path_env $ tp `mappend` ":" `mappend` pe
  where
    path_env = "PATH"
