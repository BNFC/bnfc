module BNFC.Backend.HaskellSpec where

import Test.Hspec
import BNFC.GetCF
import BNFC.Hspec
import BNFC.Options hiding (Backend)
import BNFC.Backend.Base
import Text.Printf (printf)

import BNFC.Backend.Haskell -- SUT

calcOptions = defaultOptions { lang = "Calc" }
getCalc = parseCF  calcOptions TargetHaskell $
  unlines [ "EAdd. Exp ::= Exp \"+\" Exp1  ;"
          , "ESub. Exp ::= Exp \"-\" Exp1  ;"
          , "EMul. Exp1  ::= Exp1  \"*\" Exp2  ;"
          , "EDiv. Exp1  ::= Exp1  \"/\" Exp2  ;"
          , "EInt. Exp2  ::= Integer ;"
          , "coercions Exp 2 ;" ]

spec = do

  context "with default option and the Calc grammar" $ do
    it "generates a file called AbsCalc.hs" $ do
      calc <- getCalc
      files <- execBackend (makeHaskell calcOptions calc)
      map fst files `shouldSatisfy` elem "AbsCalc.hs"

    it "generates a file called LexCalc.x" $ do
      calc <- getCalc
      files <- execBackend (makeHaskell calcOptions calc)
      map fst files `shouldSatisfy` elem "LexCalc.x"

    it "generates a file called ParCalc.y" $ do
      calc <- getCalc
      makeHaskell calcOptions calc `shouldGenerate` "ParCalc.y"

    it "generates a squeleton file" $ do
      calc <- getCalc
      makeHaskell calcOptions calc `shouldGenerate` "SkelCalc.hs"

    it "generates a pretty printer file" $ do
      calc <- getCalc
      makeHaskell calcOptions calc `shouldGenerate` "PrintCalc.hs"

    it "generates a test program file" $ do
      calc <- getCalc
      makeHaskell calcOptions calc `shouldGenerate` "TestCalc.hs"

    it "generates a error module file" $ do
      calc <- getCalc
      makeHaskell calcOptions calc `shouldGenerate` "ErrM.hs"

  context "with option -mMyMakefile and the Calc grammar" $ do
    it "generates a Makefile" $ do
      calc <- getCalc
      let options = calcOptions { make = Just "MyMakefile" }
      makeHaskell options calc `shouldGenerate` "MyMakefile"
