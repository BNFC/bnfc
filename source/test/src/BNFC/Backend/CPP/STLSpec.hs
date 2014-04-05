module BNFC.Backend.CPP.STLSpec where

import BNFC.Options
import BNFC.GetCF

import Test.Hspec
import BNFC.Hspec

import BNFC.Backend.CPP.STL -- SUT

calcOptions = defaultOptions { lang = "Calc" }
getCalc = parseCF  calcOptions TargetCpp $
  unlines [ "EAdd. Exp ::= Exp \"+\" Exp1  ;"
          , "ESub. Exp ::= Exp \"-\" Exp1  ;"
          , "EMul. Exp1  ::= Exp1  \"*\" Exp2  ;"
          , "EDiv. Exp1  ::= Exp1  \"/\" Exp2  ;"
          , "EInt. Exp2  ::= Integer ;"
          , "coercions Exp 2 ;" ]

spec =
  describe "C backend" $
    it "respect the makefile option" $ do
      calc <- getCalc
      let opts = calcOptions { make = Just "MyMakefile" }
      makeCppStl opts calc `shouldGenerate` "MyMakefile"
