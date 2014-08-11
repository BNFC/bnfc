{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Test.HUnit
import Shelly
import Prelude hiding (FilePath)
import Filesystem.Path (filename, basename)
import Filesystem.Path.CurrentOS (encodeString)
import Data.Text (Text)
import Data.Monoid ((<>))

foo x = (1,3)

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

data TestContext = TestContext
  { -- | Name of the test settings, ex 'Haskell with GADT'
    tcName        :: String
  , -- | Options to pass to bnfc
    tcBnfcOptions :: [Text]
  , -- | Command for building the generated test executable
    tcMake        :: Sh ()
  , -- | Command for running the test executable
    tcRun         :: Text -> Sh () }

data TestData = TestData { testGrammar :: FilePath, testExample :: FilePath }

settings :: [TestContext]
settings =
  [ base { tcName = "Haskell"
         , tcBnfcOptions = ["--haskell", "-m"] }
  , base { tcName = "Haskell/GADT"
         , tcBnfcOptions = ["--haskell-gadt", "-m"] }
  , base { tcName = "Haskell/CNF"
         , tcBnfcOptions = ["--haskell", "--cnf", "-m"] }
  , base { tcName = "C"
         , tcBnfcOptions = ["--c", "-m"] }
  , base { tcName = "C++"
         , tcBnfcOptions = ["--cpp", "-m"] }
  , base { tcName = "C++ (no STL)"
         , tcBnfcOptions = ["--cpp-nostl", "-m"] }
  , base { tcName = "Java"
         , tcBnfcOptions = ["--java", "-m"]
         , tcRun = \lang -> cmd ("." </> lang </> "Test") }
  ]
  where base = TestContext undefined -- name
                           undefined -- bnfc options
                           (run_ "make" []) -- make command
                           (\lang -> cmd ("." </> ("Test" <> lang))) -- run

testData :: [(FilePath, FilePath)]
testData =
  [ ("data"</>"cnf"</>"c.cf", "data"</>"cnf"</>"small.c" )
  , ("data"</>"c"</>"c.cf", "data"</>"c"</>"test.c" )
  , ("data"</>"cpp"</>"cpp.cf", "data"</>"cpp"</>"test.cpp" )
  , ("data"</>"gf"</>"gf.cf", "data"</>"gf"</>"test.gf" )
  , ("data"</>"ocl"</>"ocl.cf", "data"</>"ocl"</>"test.ocl" )
  , ("data"</>"lbnf"</>"lbnf.cf", "data"</>"lbnf"</>"test.lbnf" ) ]

mkTest :: TestContext -> (FilePath, FilePath) -> Test
mkTest context (grammar, example) =
  TestLabel label $ TestCase $ shelly $ silently $ withTmpDir $ \tmp -> do
    cp grammar tmp
    cp example tmp
    cd tmp
    let args = tcBnfcOptions context ++ [toTextArg (filename grammar)]
    run_ "bnfc" args
    tcMake context
    readfile (filename example) >>= setStdin
    tcRun context lang
  where label = encodeString grammar
        lang = toTextArg (basename grammar)
        silently = print_commands False
                 . print_stdout False
                 . print_stderr False

mkTestSuite :: TestContext -> Test
mkTestSuite context =
  TestLabel (tcName context) $ TestList (map (mkTest context) testData)

main = runTestTT $ TestList (map mkTestSuite settings)

