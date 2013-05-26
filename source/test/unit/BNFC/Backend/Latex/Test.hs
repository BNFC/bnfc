 {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module BNFC.Backend.Latex.Test where

import Shelly
import Test.Hspec
import Control.Monad (liftM)
import Prelude hiding (FilePath)
import Data.Monoid (mappend)
import Test.HUnit (assertBool)
import qualified Data.Text.Lazy as LT
default (LT.Text)

-- | This is a wrapper arround shelly that provides an easy environment
-- for testing. It changes the current dir to a temporary directory where
-- Calc.cf has been copied and adds bnfc's build path (usually dist/build/bnfc)
-- to the PATH environment variable.
shTest :: Sh a -> IO a
shTest cmds = do
  shelly $ withTmpDir $ \temp -> do
  -- We assume that the tests are run from the directory where the .cabal
  -- file is and that it is still the current working dir so we can compute
  -- the path to the needed files
  -- 1. Add bnfc's build dir to the current path
  absPath ( "dist"</>"build"</>"bnfc") >>= appendToPath
  -- cmd "export" ("PATH=" `mappend` bnfcPath `mappend` ":$PATH")
  -- 2. now we copy the Calc grammar to the temporary dir
  calc <- absPath (".." </> "examples" </> "Calc.cf")
  cp calc temp
  -- 3. Finally we move to the tmp dir and run the commands
  -- passed as an argument
  cd temp
  cmds

bnfc args = run "env" ("bnfc":args)

shouldCreate :: Sh a -> [FilePath] -> Expectation
shouldCreate command fpaths = shTest $ command >> mapM_ assertExists_sh fpaths
  where assertBool_sh msg b = liftIO $ assertBool msg b
        assertExists_sh fp
          = test_e fp >>= assertBool_sh ("File " ++ show fp ++ " wasn't created")

spec = describe "BNFC.Backend.Latex" $ do
  describe "LaTeX backend" $ do
    it "creates the .tex file" $
      bnfc ["--latex", "Calc.cf"] `shouldCreate` ["Calc.tex"]
    context "given option --makefile" $ it "creates the Makefile" $
        bnfc ["--latex", "--makefile", "Calc.cf"]
          `shouldCreate` ["Calc.tex", "Makefile"]

  describe "Haskell backend" $ do

    it "creates the correct set of files" $
      bnfc ["-haskell", "Calc.cf"]
        `shouldCreate` ["AbsCalc.hs", "LexCalc.x", "ParCalc.y", "SkelCalc.hs",
                        "PrintCalc.hs", "TestCalc.hs", "ErrM.hs" ]

    it "creates a Makefile with --makefile" $
      (bnfc ["-haskell", "-m", "Calc.cf"] >> cmd "ls")
        `shouldCreate` ["Makefile"]
