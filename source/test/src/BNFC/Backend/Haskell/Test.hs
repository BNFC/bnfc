{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module BNFC.Backend.Haskell.Test where

import Shelly
import ShellyTesting
import Test.Hspec
import qualified Data.Text.Lazy as LT

import BNFC.Backend.Latex -- SUT
default (LT.Text)

spec = describe "BNFC.Backend.Haskell" $ do

  it "creates the correct set of files" $
    bnfc ["-haskell", "Calc.cf"]
      `shouldCreate` ["AbsCalc.hs", "LexCalc.x", "ParCalc.y", "SkelCalc.hs",
                      "PrintCalc.hs", "TestCalc.hs", "ErrM.hs" ]

  it "creates a Makefile with --makefile" $
    (bnfc ["-haskell", "-m", "Calc.cf"] >> cmd "ls")
      `shouldCreate` ["Makefile"]
