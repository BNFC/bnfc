module BNFC.Backend.LatexSpec where

import Test.Hspec
import Test.QuickCheck
import System.FilePath ((<.>))
import BNFC.WarningM
import Control.Monad (liftM2)

import BNFC.Backend.Latex -- SUT

spec :: Spec
spec = describe "BNFC.Backend.Latex" $ do

  describe "makefile" $ do

    it "creates a makefile for the given tex file" $
      makefile "myFile.tex" `shouldBe`
        unlines [ "all: myFile.pdf",
                  "",
                  "myFile.pdf: myFile.tex",
                  "\tpdflatex myFile.tex",
                  "",
                  "clean:",
                  "\t-rm myFile.pdf myFile.aux myFile.log",
                  "",
                  "cleanall: clean",
                  "\t-rm Makefile myFile.tex",
                  "" ]
