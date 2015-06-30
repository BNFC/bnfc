module BNFC.Backend.LatexSpec where

import BNFC.Options
import BNFC.GetCF

import Test.Hspec
import BNFC.Hspec

import BNFC.Backend.Latex -- SUT

calcOptions = defaultOptions { lang = "Calc" }
getCalc = parseCF  calcOptions TargetLatex $
  unlines [ "EAdd. Exp ::= Exp \"+\" Exp1  ;"
          , "ESub. Exp ::= Exp \"-\" Exp1  ;"
          , "EMul. Exp1  ::= Exp1  \"*\" Exp2  ;"
          , "EDiv. Exp1  ::= Exp1  \"/\" Exp2  ;"
          , "EInt. Exp2  ::= Integer ;"
          , "coercions Exp 2 ;" ]

spec = do

  describe "LaTeX backend" $ do
    it "creates the .tex file" $ do
      calc <- getCalc
      makeLatex calcOptions calc `shouldGenerate` "Calc.tex"

    it "creates the Makefile" $ do
      calc <- getCalc
      let options = calcOptions { make = Just "Makefile" }
      makeLatex options calc `shouldGenerate` "Makefile"

  describe "prt" $ do
    it "correctly converts ^^ into latex \textasciicircum\textasciicircum" $
      prt "^^" `shouldBe` "{\\textasciicircum}{\\textasciicircum}"
