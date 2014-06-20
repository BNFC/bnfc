module BNFC.Backend.CSharpSpec where

import BNFC.Options
import BNFC.GetCF

import Test.Hspec
import BNFC.Hspec

import BNFC.Backend.CSharp -- SUT

calcOptions = defaultOptions { lang = "Calc" }
getCalc = parseCF  calcOptions TargetCSharp $
  unlines [ "EAdd. Exp ::= Exp \"+\" Exp1  ;"
          , "ESub. Exp ::= Exp \"-\" Exp1  ;"
          , "EMul. Exp1  ::= Exp1  \"*\" Exp2  ;"
          , "EDiv. Exp1  ::= Exp1  \"/\" Exp2  ;"
          , "EInt. Exp2  ::= Integer ;"
          , "coercions Exp 2 ;" ]

spec =
  describe "C# backend" $
    it "respect the makefile option" $ do
      calc <- getCalc
      let opts = calcOptions { make = Just "MyMakefile" }
      makeCSharp opts calc `shouldGenerate` "MyMakefile"

