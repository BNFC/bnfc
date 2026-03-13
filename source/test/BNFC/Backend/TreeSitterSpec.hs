module BNFC.Backend.TreeSitterSpec where

import qualified Paths_BNFC

import System.FilePath
import System.Directory(listDirectory)

import qualified Data.List as List

import BNFC.Backend.Base(fileName, execBackend)
import BNFC.Options
import BNFC.GetCF

import Test.Hspec
import Test.HUnit ((@?))
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

listDataFiles = do
  dataDir <- Paths_BNFC.getDataDir
  let dir = dataDir </> "test/BNFC/Backend/TreeSitter"

  files <- listDirectory dir
  pure $ map (dir </>) $ filter ((== ".cf") . takeExtension) files

runFileTest filename = do
  let opts = (defaultOptions { lang = takeBaseName filename})

  bnfc <- readFile (filename -<.> "cf")
  expected <- readFile (filename -<.> "expected.js")

  cf <- parseCF opts TargetTreeSitter bnfc
  let backend = makeTreeSitter opts cf

  -- get the name of the grammar.js file within a subfolder
  fileNames <- map fileName <$> execBackend backend
  let (Just grammarJs) = List.find ((== "grammar.js") . takeFileName) fileNames

  backend `shouldGenerateText` (grammarJs, expected)

makeFileTest filename =
  it ("tree-sitter expect test: " ++ filename) $
    runFileTest filename

spec = do

  describe "Tree-Sitter backend" $ do
    it "creates the grammar.js file" $ do
      calc <- getCalc
      makeTreeSitter calcOptions calc `shouldGenerate` ("tree-sitter-calc" </> "grammar.js")

    cfFiles <- runIO listDataFiles

    it "should find at least one expect test" $ do
      not (null cfFiles) @? "no .cf files found"

    mapM_ makeFileTest cfFiles
