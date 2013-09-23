{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit
import Shelly
import qualified Data.Text.Lazy as T
default (T.Text)

main = defaultMain tests

tests = [
  testCase "#60 Compilation error in Java when a production uses more than one user-defined tokens" $
    shelly $ silently $ withTmpDir $ \tmp -> do
      bnfc <- absPath ( "dist"</>"build"</>"bnfc"</>"bnfc")
      cd tmp
      writefile "multiple_token.cf" $ T.unlines
        [ "Label. Category ::= FIRST SECOND;"
        , "token FIRST 'a';"
        , "token SECOND 'b';" ]
      cmd bnfc "--java" "-m" "multiple_token.cf"
      cmd "make"
  , testCase "#30 With -d option XML module is not generated inside the directorty" $ do
        shelly $ silently $ withTmpDir $ \tmp -> do
          bnfc <- absPath ( "dist"</>"build"</>"bnfc"</>"bnfc")
          cd tmp
          writefile "Test.cf" $ T.unlines
            [ "Start. S ::= S \"a\" ;"
            , "End.   S ::= ;" ]
          cmd bnfc "--haskell" "--xml" "-m" "-d" "Test.cf"
          test_f "Test/XML.hs" >>= liftIO . assertBool "can't find file Test/XML.hs"
          cmd "make"
  ]
