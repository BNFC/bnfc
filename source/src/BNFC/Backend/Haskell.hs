{-
    BNF Converter: Haskell main file
    Copyright (C) 2004  Author:  Markus Forberg, Peter Gammie, Aarne Ranta

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

module BNFC.Backend.Haskell (makeHaskell, AlexVersion(..), makefile, testfile) where



-- import BNFC.Utils
import BNFC.Options hiding (Backend)
import BNFC.CF
import BNFC.Backend.Base
import BNFC.Backend.Haskell.CFtoHappy
import BNFC.Backend.Haskell.CFtoAlex
import BNFC.Backend.Haskell.CFtoAlex2
import BNFC.Backend.Haskell.CFtoAlex3
import BNFC.Backend.Txt2Tag
import BNFC.Backend.Haskell.CFtoAbstract
import BNFC.Backend.Haskell.CFtoTemplate
import BNFC.Backend.Haskell.CFtoPrinter
import BNFC.Backend.Haskell.CFtoLayout
import BNFC.Backend.XML
import BNFC.Backend.Haskell.HsOpts
import BNFC.Backend.Haskell.ToCNF as ToCNF
import BNFC.Backend.Haskell.MkErrM
import BNFC.Backend.Haskell.MkSharedString
import BNFC.Backend.Haskell.Utils (parserName)
import qualified BNFC.Backend.Common.Makefile as Makefile

import System.FilePath (pathSeparator)
import Control.Monad(when,unless)
import Text.Printf (printf)
import Text.PrettyPrint

-- naming conventions




makeHaskell :: SharedOptions -> CF -> Backend
makeHaskell opts cf = do
  let absMod = absFileM opts
      lexMod = alexFileM opts
      parMod = happyFileM opts
      prMod  = printerFileM opts
      layMod = layoutFileM opts
      errMod = errFileM opts
      shareMod = shareFileM opts
  do
    mkfile (absFile opts) $ cf2Abstract (byteStrings opts) (ghcExtensions opts) (functor opts) absMod cf
    case alexMode opts of
      Alex1 -> do
        mkfile (alexFile opts) $ cf2alex lexMod errMod cf
        liftIO $ printf "Use Alex 1.1 to compile %s.\n" (alexFile opts)
      Alex2 -> do
        mkfile (alexFile opts) $ cf2alex2 lexMod errMod shareMod (shareStrings opts) (byteStrings opts) cf
        liftIO $ printf "Use Alex 2.0 to compile %s.\n" (alexFile opts)
      Alex3 -> do
        mkfile (alexFile opts) $ cf2alex3 lexMod errMod shareMod (shareStrings opts) (byteStrings opts) cf
        liftIO $ printf "Use Alex 3.0 to compile %s.\n" (alexFile opts)
    unless (cnf opts) $ do
      mkfile (happyFile opts) $
        cf2HappyS parMod absMod lexMod errMod (glr opts) (byteStrings opts) (functor opts) cf
      liftIO $ printf "%s Tested with Happy 1.15\n" (happyFile opts)
    mkfile (tFile opts)        $ testfile opts cf
    mkfile (txtFile opts)      $ cfToTxt (lang opts) cf
    mkfile (templateFile opts) $ cf2Template (templateFileM opts) absMod errMod (functor opts) cf
    mkfile (printerFile opts)  $ cf2Printer (byteStrings opts) (functor opts) False prMod absMod cf
    when (hasLayout cf) $ mkfile (layoutFile opts) $ cf2Layout (alex1 opts) (inDir opts) layMod lexMod cf
    mkfile (errFile opts) $ mkErrM errMod (ghcExtensions opts)
    when (shareStrings opts) $ mkfile (shareFile opts)    $ sharedString shareMod (byteStrings opts) cf
    Makefile.mkMakefile opts $ makefile opts
    case xml opts of
      2 -> makeXML opts True cf
      1 -> makeXML opts False cf
      _ -> return ()
    when (cnf opts) $ do
      mkfile (cnfTablesFile opts) $ ToCNF.generate opts cf
      mkfile "TestCNF.hs" $ ToCNF.genTestFile opts cf
      mkfile "BenchCNF.hs" $ ToCNF.genBenchmark opts


makefile :: Options -> Doc
makefile opts = makeA where
  glr_params = if glr opts == GLR then "--glr --decode " else ""
  dir = let d = codeDir opts in if null d then "" else d ++ [pathSeparator]
  makeA = vcat
      [ Makefile.mkRule "all" []
           ([ "happy -gca " ++ glr_params ++ happyFile opts | not (cnf opts) ] ++
            [ "alex -g " ++ alexFile opts ] ++
            [ if cnf opts
              then "ghc --make TestCNF.hs"
              else "ghc --make " ++ tFile opts ++ " -o " ++ mkFile withLang "Test" "" opts])
      , Makefile.mkRule "clean" []
          [ "-rm -f "  ++ unwords
                (map (dir++) [ "*.log", "*.aux", "*.hi", "*.o", "*.dvi" ]) ]
      ,  Makefile.mkRule "distclean" ["clean"]
          [ "-rm -f " ++ unwords
              [ mkFile withLang "Doc" "*" opts
              , mkFile withLang "Lex" "*" opts
              , mkFile withLang "Par" "*" opts
              , mkFile withLang "Layout" "*" opts
              , mkFile withLang "Skel" "*" opts
              , mkFile withLang "Print" "*" opts
              , mkFile withLang "Test" "*" opts
              , mkFile withLang "Abs" "*" opts
              , mkFile withLang "Test" "" opts
              , mkFile noLang   "ErrM" "*" opts
              , mkFile noLang   "SharedString" "*" opts
              , mkFile noLang "ComposOp" "*" opts
              , dir ++ lang opts ++ ".dtd"
              , mkFile withLang "XML" "*" opts
              , "Makefile*" ]
          , if null dir then "" else "\t-rmdir -p " ++ dir ]
      ]

testfile :: Options -> CF -> String
testfile opts cf
        = let lay = hasLayout cf
              use_xml = xml opts > 0
              xpr = if use_xml then "XPrint a, "     else ""
              use_glr = glr opts == GLR
              if_glr s = if use_glr then s else ""
              firstParser = if use_glr then "the_parser" else render (parserName topType)
              topType = firstEntry cf
          in unlines
                ["-- automatically generated by BNF Converter",
                 "module Main where\n",
                 "",
                 "import System.IO ( stdin, hGetContents )",
                 "import System.Environment ( getArgs, getProgName )",
                 "import System.Exit ( exitFailure, exitSuccess )",
                 "import Control.Monad (when)",
                 "",
                 "import " ++ alexFileM     opts,
                 "import " ++ happyFileM    opts,
                 "import " ++ templateFileM opts,
                 "import " ++ printerFileM  opts,
                 "import " ++ absFileM      opts,
                 if lay then "import " ++ layoutFileM opts else "",
                 if use_xml then "import " ++ xmlFileM opts else "",
                 if_glr "import qualified Data.Map(Map, lookup, toList)",
                 if_glr "import Data.Maybe(fromJust)",
                 "import " ++ errFileM      opts,
                 "",
                 if use_glr
                   then "type ParseFun a = [[Token]] -> (GLRResult, GLR_Output (Err a))"
                   else "type ParseFun a = [Token] -> Err a",
                 "",
                 "myLLexer = " ++ if lay then "resolveLayout True . myLexer"
                                         else "myLexer",
                 "",
                 "type Verbosity = Int",
                 "",
                 "putStrV :: Verbosity -> String -> IO ()",
                 "putStrV v s = when (v > 1) $ putStrLn s",
                 "",
                 "runFile :: (" ++ xpr ++ if_glr "TreeDecode a, " ++ "Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()",
                 "runFile v p f = putStrLn f >> readFile f >>= run v p",
                 "",
                 "run :: (" ++ xpr ++ if_glr "TreeDecode a, " ++ "Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()",
                 if use_glr then runGlr else runStd use_xml,
                 "",
                 "showTree :: (Show a, Print a) => Int -> a -> IO ()",
                 "showTree v tree",
                 " = do",
                 "      putStrV v $ \"\\n[Abstract Syntax]\\n\\n\" ++ show tree",
                 "      putStrV v $ \"\\n[Linearized tree]\\n\\n\" ++ printTree tree",
                 "",
                 "usage :: IO ()",
                 "usage = do",
                 "  putStrLn $ unlines",
                 "    [ \"usage: Call with one of the following argument combinations:\"",
                 "    , \"  --help          Display this help message.\"",
                 "    , \"  (no arguments)  Parse stdin verbosely.\"",
                 "    , \"  (files)         Parse content of files verbosely.\"",
                 "    , \"  -s (files)      Silent mode. Parse content of files silently.\"",
                 "    ]",
                 "  exitFailure",
                 "",
                 "main :: IO ()",
                 "main = do",
                 "  args <- getArgs",
                 "  case args of",
                 "    [\"--help\"] -> usage",
                 "    [] -> getContents >>= run 2 " ++ firstParser,
                 "    \"-s\":fs -> mapM_ (runFile 0 " ++ firstParser ++ ") fs",
                 "    fs -> mapM_ (runFile 2 " ++ firstParser ++ ") fs",
                 "",
                 if_glr $ "the_parser :: ParseFun " ++ show topType,
                 if_glr $ "the_parser = lift_parser " ++ render (parserName topType),
                 if_glr "",
                 if_glr liftParser
                 ]

runStd xml
 = unlines
   [ "run v p s = let ts = myLLexer s in case p ts of"
   , "           Bad s    -> do putStrLn \"\\nParse              Failed...\\n\""
   , "                          putStrV v \"Tokens:\""
   , "                          putStrV v $ show ts"
   , "                          putStrLn s"
   , "                          exitFailure"
   , "           Ok  tree -> do putStrLn \"\\nParse Successful!\""
   , "                          showTree v tree"
   , if xml then
     "                          putStrV v $ \"\\n[XML]\\n\\n\" ++ printXML tree"
     else ""
   , "                          exitSuccess"
   ]

runGlr
 = unlines
   [ "run v p s"
   , " = let ts = map (:[]) $ myLLexer s"
   , "       (raw_output, simple_output) = p ts in"
   , "   case simple_output of"
   , "     GLR_Fail major minor -> do"
   , "                               putStrLn major"
   , "                               putStrV v minor"
   , "     GLR_Result df trees  -> do"
   , "                               putStrLn \"\\nParse Successful!\""
   , "                               case trees of"
   , "                                 []       -> error \"No results but parse succeeded?\""
   , "                                 [Ok x]   -> showTree v x"
   , "                                 xs@(_:_) -> showSeveralTrees v xs"
   , "   where"
   , "  showSeveralTrees :: (Print b, Show b) => Int -> [Err b] -> IO ()"
   , "  showSeveralTrees v trees"
   , "   = sequence_ "
   , "     [ do putStrV v (replicate 40 '-')"
   , "          putStrV v $ \"Parse number: \" ++ show n"
   , "          showTree v t"
   , "     | (Ok t,n) <- zip trees [1..]"
   , "     ]"
   ]


liftParser
 = unlines
   [ "type Forest = Data.Map.Map ForestId [Branch]      -- omitted in ParX export."
   , "data GLR_Output a"
   , " = GLR_Result { pruned_decode     :: (Forest -> Forest) -> [a]"
   , "              , semantic_result   :: [a]"
   , "              }"
   , " | GLR_Fail   { main_message :: String"
   , "              , extra_info   :: String"
   , "              }"
   , ""
   , "lift_parser"
   , " :: (TreeDecode a, Show a, Print a)"
   , " => ([[Token]] -> GLRResult) -> ParseFun a"
   , "lift_parser parser ts"
   , " = let result = parser ts in"
   , "   (\\o -> (result, o)) $"
   , "   case result of"
   , "     ParseError ts f -> GLR_Fail \"Parse failed, unexpected token(s)\\n\""
   , "                                 (\"Tokens: \" ++ show ts)"
   , "     ParseEOF   f    -> GLR_Fail \"Parse failed, unexpected EOF\\n\""
   , "                                 (\"Partial forest:\\n\""
   , "                                    ++ unlines (map show $ Data.Map.toList f))"
   , "     ParseOK r f     -> let find   f = fromJust . ((flip Data.Map.lookup) f)"
   , "                            dec_fn f = decode (find f) r"
   , "                        in GLR_Result (\\ff -> dec_fn $ ff f) (dec_fn f)"
   ]
