{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module BNFC.Backend.LatexSpec where

import Shelly
import ShellyTesting
import Test.Hspec
import qualified Data.Text.Lazy as LT

import BNFC.Backend.Latex -- SUT
default (LT.Text)

spec = do

  describe "makefile" $ do
    it "creates a makefile for the given tex file" $
      makefile "myFile.tex" `shouldBe`
        unlines [ "all: myFile.pdf", "",
                  "myFile.pdf: myFile.tex",
                  "\tpdflatex myFile.tex", "",
                  "clean:",
                  "\t-rm myFile.pdf myFile.aux myFile.log", "",
                  "cleanall: clean",
                  "\t-rm Makefile myFile.tex",
                  "" ]
  describe "LaTeX backend" $ do

    it "creates the .tex file" $
      bnfc ["--latex", "Calc.cf"] `shouldCreate` ["Calc.tex"]
    it "creates the Makefile" $
      bnfc ["--latex", "--makefile", "Calc.cf"]
        `shouldCreate` ["Calc.tex", "Makefile"]
