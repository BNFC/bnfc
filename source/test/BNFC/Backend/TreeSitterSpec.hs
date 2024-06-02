module BNFC.Backend.TreeSitterSpec where

import BNFC.Options
import BNFC.GetCF

import Test.Hspec
import BNFC.Hspec

import BNFC.Backend.TreeSitter -- SUT

calcOptions = defaultOptions { lang = "Calc" }
getCalc = parseCF  calcOptions TargetTreeSitter $
  unlines [ "EAdd. Exp ::= Exp \"+\" Exp1  ;"
          , "ESub. Exp ::= Exp \"-\" Exp1  ;"
          , "EMul. Exp1  ::= Exp1  \"*\" Exp2  ;"
          , "EDiv. Exp1  ::= Exp1  \"/\" Exp2  ;"
          , "EInt. Exp2  ::= Integer ;"
          , "coercions Exp 2 ;" ]

spec = do

  describe "Tree-Sitter backend" $ do
    it "creates the grammar.js file" $ do
      calc <- getCalc
      makeTreeSitter calcOptions calc `shouldGenerate` "grammar.js"
