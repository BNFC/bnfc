{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module BNFC.Backend.BaseSpec where

import Control.Monad (liftM, liftM2)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.Directory
import System.FilePath ((<.>), takeBaseName)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import BNFC.Backend.Base -- SUT

spec :: Spec
spec = do
  describe "Backend monad" $ do
    it "empty computation generates empty list op files" $
      execBackend (return ()) `shouldReturn` []
    it "returns the file created using mkfile" $
      execBackend (mkfile "test.txt" "abcd")
        `shouldReturn` [("test.txt", "abcd")]
  describe "writeFiles" $ do
    it "raises an exception if the root directory does not exists" $
      withSystemTempDirectory "bnfc-test" (\tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "I/dont/exists" (return ()))
      `shouldThrow` anyException
    it "creates a file from the bucket" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "." (mkfile "file.txt" "")
        doesFileExist "file.txt"
      `shouldReturn` True
    it "put the right content in the file" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "." (mkfile "file.txt" "abcd")
        readFile "file.txt"
      `shouldReturn` "abcd"
    it "creates subdirectories" $ 
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "." (mkfile "subdir/file.txt" "abcd")
        doesDirectoryExist "subdir"
      `shouldReturn` True
