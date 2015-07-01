{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
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

default(String)

spec :: Spec
spec = do
  describe "Backend monad" $ do
    it "empty computation generates empty list op files" $
      execBackend (return ()) `shouldReturn` []
    it "returns the file created using mkfile" $
      execBackend (mkfile "test.txt" "abcd")
        `shouldReturn` [("test.txt", "abcd")]
  describe "writeFiles" $ do
    it "creates the root directory if it doesn't exists" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "foo/bar" (return ())
        doesDirectoryExist "foo/bar" `shouldReturn` True
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
    it "creates files in the root directory" $
      withSystemTempDirectory "bnfc-test" $ \tmpdir -> do
        setCurrentDirectory tmpdir
        writeFiles "root/" (mkfile "foo/bar.txt" "abcd")
        doesFileExist "root/foo/bar.txt" `shouldReturn` True
