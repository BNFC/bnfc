{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Test.Framework
import qualified BNFC.Backend.Common.Makefile as Makefile
import qualified BNFC.Options as O
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import Control.Exception (assert)
import System.FilePath ((<.>))
import Debug.Trace (trace)
import qualified BNFC.WarningM as WithWarnings
import BNFC.WarningM (WithWarnings)

main = htfMain htf_thisModulesTests

test_mkRule1 = assertEqual
  "main: file1 file2\n\tdo something\n\n"
  $ Makefile.mkRule "main" ["file1","file2"] ["do something"] ""

test_mkRule2 = assertEqual
  "main: program.exe\n\n"
  $ Makefile.mkRule "main" ["program.exe"] [] ""

test_mkDoc = assertEqual
  "doc: test.pdf\n\ntest.pdf: test.tex\n\tpdflatex test.tex\n\n"
  $ Makefile.mkDoc "test.tex" ""

-- ------------------------------------------------------------------------- --
-- Option parsing
-- ------------------------------------------------------------------------- --
shuffle :: (Eq a) => [a] -> Gen [a]
shuffle [] = return []
shuffle xs = do
  x  <- elements xs
  ys <- shuffle $ List.delete x xs
  assert (length ys == length xs - 1) $
    return (x:ys)

instance Arbitrary O.SharedOptions where
  arbitrary = do
    target <- elements [ O.TargetC, O.TargetCPP, O.TargetCPP_STL, O.TargetCSharp
                       , O.TargetHaskell, O.TargetHaskellGADT, O.TargetJava15
                       , O.TargetJava, O.TargetOCAML, O.TargetProfile ]
    make <- arbitrary
    alexMode <- elements [O.Alex1,O.Alex2,O.Alex3]
    inDir <- arbitrary
    shareStrings <- arbitrary
    byteStrings <- arbitrary
    glr <- elements [ O.Standard, O.GLR ]
    xml <- elements [ 0, 1, 2 ]
    inPackage <- return Nothing
    lang <- listOf1 $ elements ['A'..'z']
    multi <- return False
    cnf <- return False
    return $ O.Options [target] make alexMode inDir shareStrings byteStrings glr
                       xml inPackage lang multi cnf

optionsToArguments :: O.SharedOptions -> [String]
optionsToArguments (O.Options
  [target] make alexMode inDir shareStrings byteStrings glr xml inPackage
  lang multi cnf)
  = [ case target of
        O.TargetC           -> "-c"
        O.TargetCPP         -> "-cpp_no_stl"
        O.TargetCPP_STL     -> "-cpp"
        O.TargetCSharp      -> "-csharp"
        O.TargetHaskell     -> "-haskell"
        O.TargetHaskellGADT -> "-gadt"
        O.TargetJava15      -> "-java"
        O.TargetJava        -> "-java1.4"
        O.TargetOCAML       -> "-ocaml"
        O.TargetProfile     -> "-prof"
    , case alexMode of
        O.Alex1 -> "-alex1"
        O.Alex2 -> "-alex2"
        O.Alex3 -> "-alex3" ]
    ++ ["-m" | make ]
    ++ ["-d" | inDir ]
    ++ ["-sharestrings" | shareStrings]
    ++ ["-bytestrings" | byteStrings ]
    ++ ["-glr" | glr == O.GLR ]
    ++ ["-xml" | xml == 1 ] ++ ["-xmlt" | xml == 2 ]

prop_correctOptionParsing :: O.SharedOptions -> Property
prop_correctOptionParsing o =
  let args = optionsToArguments o in
  forAll (shuffle args) $ \args' ->
    let o' = (fst . fromRight . O.parseArguments) (args'++[ O.lang o <.> "cf"])
    in o' == o
  where fromRight (Right s) = s

prop_isCfFile :: Property
prop_isCfFile = forAll genFilename (\n -> O.isCfFile n)
  where genFilename = do
          ext  <- elements O.allowed_exts
          name <- listOf1 $ elements ['a'..'z']
          return (name <.> ext)

prop_translateArguments :: Property
prop_translateArguments =
  forAll genArgument $ \arg ->
    arg `elem` deprecated ==> WithWarnings.hasWarnings (O.translateArguments [arg])
  where genArgument = elements (deprecated ++ others)
        deprecated =  [ "-java","-java1.5","-java1.4","-c","-cpp","-cpp_stl"
                      , "-cpp_no_stl","-csharp","-ocaml","-haskell"
                      , "-prof","-gadt","-alex1","-alex1","-alex3"
                      , "-sharestrings","-bytestrings","-glr","-xml","-xmlt"
                      , "-vs","-wcf" ]
        others =  [ "--java","--java5","--java4","--c","--cpp","--stl"
                      , "--no-stl","--csharp","--ocaml","--haskell"
                      , "--prof","--gadt","--alex1","--alex2","--alex3"
                      , "--sharestrings","--bytestrings","--glr","--xml","--xmlt"
                      , "--vs","--wcf", "-d", "-p", "-l" ]

-- ------------------------------------------------------------------------- --
-- Warning monad
-- ------------------------------------------------------------------------- --

test_run = assertEqual (3,["Coucou", "Hi"])
  $ WithWarnings.run
      (WithWarnings.warn "Coucou" >> WithWarnings.warn "Hi" >> return 3)

test_putWarnings =
  WithWarnings.putWarnings
    (WithWarnings.warn "Coucou" >> WithWarnings.warn "Hi" >> return 3)
    >>= assertEqual 3

test_hasWarnings =
  assertBool $ WithWarnings.hasWarnings
    (WithWarnings.warn "Coucou" >> WithWarnings.warn "Hi" >> return 3)

