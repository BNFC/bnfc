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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module HaskellTop (makeAll, AlexMode(..)) where 



-- import Utils
import Options
import CF
import CFtoHappy
import CFtoAlex
import CFtoAlex2
import CFtoAlex3
import CFtoLatex
import CFtoTxt
import CFtoAbstract
import CFtoTemplate
import CFtoPrinter
import CFtoLayout
import CFtoXML
import HsOpts
import ToCNF
import MkErrM
import MkSharedString
import Utils
import qualified Common.Makefile as Makefile
import Data.Char
import Data.Maybe (fromMaybe,maybe)
import System.Exit (exitFailure)
import Control.Monad(when)

-- naming conventions




makeAll :: Options -> CF -> IO ()
makeAll opts cf = do
  let absMod = absFileM opts
      lexMod = alexFileM opts
      parMod = happyFileM opts
      prMod  = printerFileM opts
      layMod = layoutFileM opts
      errMod = errFileM opts
      shareMod = shareFileM opts
  do
    let dir = codeDir opts
    when (not (null dir)) $ do
			    putStrLn $ "Creating directory " ++ dir
			    prepareDir dir
    writeFileRep (absFile opts) $ cf2Abstract (byteStrings opts) absMod cf
    case alexMode opts of
      Alex1 -> do
        writeFileRep (alexFile opts) $ cf2alex lexMod errMod cf
        putStrLn "   (Use Alex 1.1 to compile.)" 
      Alex2 -> do
        writeFileRep (alexFile opts) $ cf2alex2 lexMod errMod shareMod (shareStrings opts) (byteStrings opts) cf
        putStrLn "   (Use Alex 2.0 to compile.)"
      Alex3 -> do
        writeFileRep (alexFile opts) $ cf2alex3 lexMod errMod shareMod (shareStrings opts) (byteStrings opts) cf
        putStrLn "   (Use Alex 3.0 to compile.)"
    writeFileRep (happyFile opts) $ 
		 cf2HappyS parMod absMod lexMod errMod (glr opts) (byteStrings opts) cf
    putStrLn "   (Tested with Happy 1.15)"
    writeFileRep (latexFile opts)    $ cfToLatex (lang opts) cf
    writeFileRep (txtFile opts)      $ cfToTxt (lang opts) cf
    writeFileRep (templateFile opts) $ cf2Template (templateFileM opts) absMod errMod cf
    writeFileRep (printerFile opts)  $ cf2Printer (byteStrings opts) prMod absMod cf
    when (hasLayout cf) $ writeFileRep (layoutFile opts) $ cf2Layout (alex1 opts) (inDir opts) layMod lexMod cf
    writeFileRep (tFile opts)        $ testfile opts cf
    writeFileRep (errFile opts)      $ errM errMod cf
    when (shareStrings opts) $ writeFileRep (shareFile opts)    $ sharedString shareMod (byteStrings opts) cf
    when (make opts) $ writeFileRep "Makefile" $ makefile opts
    case xml opts of
      2 -> makeXML (lang opts) True cf
      1 -> makeXML (lang opts) False cf
      _ -> return ()
    when (cnf opts) $ do 
      writeFileRep (cnfTablesFile opts) $ ToCNF.generate opts cf
      writeFileRep "TestCNF.hs" $ ToCNF.genTestFile opts cf
      writeFileRep "BenchCNF.hs" $ ToCNF.genBenchmark opts

codeDir :: Options -> FilePath
codeDir opts = let pref = maybe "" pkgToDir (inPackage opts)
		   dir = if inDir opts then lang opts else ""
		   sep = if null pref || null dir then "" else [pathSep]
		 in pref ++ sep ++ dir 

makefile :: Options -> String
makefile opts = makeA where
  glr_params = if glr opts == GLR then "--glr --decode " else ""  
  dir = let d = codeDir opts in if null d then "" else d ++ [pathSep]
  cd c = if null dir then c else "(cd " ++ dir ++ "; " ++ c ++ ")"
  makeA = Makefile.mkRule "all" []
            [ "happy -gca " ++ glr_params ++ happyFile opts
            , "alex -g " ++ alexFile opts
            , "ghc --make " ++ tFile opts ++ " -o " ++ mkFile withLang "Test" "" opts]
        $ Makefile.mkDoc (basename (latexFile opts))
        $ Makefile.mkRule "clean" []
            [ "-rm -f "  ++ unwords 
                (map (dir++) [ "*.log", "*.aux", "*.hi", "*.o", "*.dvi" ])
            , "-rm -f " ++ psFile opts ]
        $  Makefile.mkRule "distclean" ["clean"]
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
                , dir ++ lang opts ++ ".dtd"
                , mkFile withLang "XML" "*" opts 
                , "Makefile*" ]
            , if null dir then "" else "\t-rmdir -p " ++ dir ]
        $ ""

testfile :: Options -> CF -> String
testfile opts cf
        = let lay = hasLayout cf 
	      use_xml = xml opts > 0
              xpr = if use_xml then "XPrint a, "     else ""
	      use_glr = glr opts == GLR
              if_glr s = if use_glr then s else ""
	      firstParser = if use_glr then "the_parser" else parserName
	      parserName = 'p' : topType
	      topType = firstEntry cf
          in unlines
	        ["-- automatically generated by BNF Converter",
		 "module Main where\n",
	         "",
	         "import System.IO ( stdin, hGetContents )",
	         "import System.Environment ( getArgs, getProgName )",
		 "",
		 "import " ++ alexFileM     opts,
		 "import " ++ happyFileM    opts,
		 "import " ++ templateFileM opts,
	         "import " ++ printerFileM  opts,
	         "import " ++ absFileM      opts,
	         if lay then ("import " ++ layoutFileM opts) else "",
	         if use_xml then ("import " ++ xmlFileM opts) else "",
	         if_glr "import Data.FiniteMap(FiniteMap, lookupFM, fmToList)",
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
                 "putStrV v s = if v > 1 then putStrLn s else return ()",
                 "",
		 "runFile :: (" ++ xpr ++ if_glr "TreeDecode a, " ++ "Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()",
		 "runFile v p f = putStrLn f >> readFile f >>= run v p",
		 "",
		 "run :: (" ++ xpr ++ if_glr "TreeDecode a, " ++ "Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()",
		 if use_glr then run_glr else run_std use_xml,
		 "",
		 "showTree :: (Show a, Print a) => Int -> a -> IO ()",
		 "showTree v tree",
		 " = do",
		 "      putStrV v $ \"\\n[Abstract Syntax]\\n\\n\" ++ show tree",
		 "      putStrV v $ \"\\n[Linearized tree]\\n\\n\" ++ printTree tree",
		 "",
		 "main :: IO ()",
		 "main = do args <- getArgs",
		 "          case args of",
		 "            [] -> hGetContents stdin >>= run 2 " ++ firstParser,
		 "            \"-s\":fs -> mapM_ (runFile 0 " ++ firstParser ++ ") fs",
		 "            fs -> mapM_ (runFile 2 " ++ firstParser ++ ") fs",
		 "",
		 if_glr $ "the_parser :: ParseFun " ++ topType,
		 if_glr $ "the_parser = lift_parser " ++ parserName,
		 if_glr $ "",
		 if_glr $ lift_parser
		 ]

run_std xml
 = unlines 
   [ "run v p s = let ts = myLLexer s in case p ts of"
   , "           Bad s    -> do putStrLn \"\\nParse              Failed...\\n\""
   , "                          putStrV v \"Tokens:\""
   , "                          putStrV v $ show ts"
   , "                          putStrLn s"
   , "           Ok  tree -> do putStrLn \"\\nParse Successful!\""
   , "                          showTree v tree"
   , if xml then
     "                          putStrV v $ \"\\n[XML]\\n\\n\" ++ printXML tree"
     else ""
   ]

run_glr
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
   

lift_parser
 = unlines 
   [ "type Forest = FiniteMap ForestId [Branch]      -- omitted in ParX export."
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
   , "                                    ++ unlines (map show $ fmToList f))"
   , "     ParseOK r f     -> let find   f = fromJust . lookupFM f"
   , "                            dec_fn f = decode (find f) r"
   , "                        in GLR_Result (\\ff -> dec_fn $ ff f) (dec_fn f)"
   ]

