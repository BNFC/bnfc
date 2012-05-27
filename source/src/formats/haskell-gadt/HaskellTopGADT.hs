{-
    BNF Converter: Haskell main file
    Copyright (C) 2004-2005  Author:  Markus Forberg, Peter Gammie, 
                                      Aarne Ranta, Björn Bringert

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

module HaskellTopGADT (makeAllGADT) where 



-- import Utils
import CF
import CFtoHappy
import CFtoAlex
import CFtoAlex2
import CFtoAlex3
import CFtoLatex
import HaskellTop(AlexMode(..))
import CFtoAbstractGADT
import CFtoTemplateGADT
import CFtoPrinterGADT
import CFtoLayout
import CFtoXML
import MkErrM
import MkSharedString
-- import CFtoGF		( cf2AbsGF, cf2ConcGF )
import GetCF
import Utils

import Data.Char
import Data.Maybe (fromMaybe,maybe)
import System.Exit (exitFailure)
import Control.Monad(when)

-- naming conventions

noLang :: Options -> String -> String
noLang _ name = name

withLang :: Options -> String -> String
withLang opts name = name ++ lang opts

mkMod :: (Options -> String -> String) -> String -> Options -> String
mkMod addLang name opts = 
    pref ++ if inDir opts then lang opts ++ "." ++ name else addLang opts name
	where pref = maybe "" (++".") (inPackage opts)

mkFile :: (Options -> String -> String) -> String -> String -> Options -> FilePath
mkFile addLang name ext opts = 
    pref ++ if inDir opts
       then lang opts ++ [pathSep] ++ name ++ ext'
       else addLang opts name ++ if null ext then "" else ext'
    where pref = maybe "" (\p->pkgToDir p++[pathSep]) (inPackage opts)
	  ext' = if null ext then "" else "." ++ ext

absFile, absFileM, alexFile, alexFileM, dviFile,
 composOpFile, composOpFileM,
 gfAbs, gfConc,
 happyFile, happyFileM,
 latexFile, errFile, errFileM,
 templateFile, templateFileM, 
 printerFile, printerFileM,
 layoutFile, layoutFileM, 
 psFile, tFile, tFileM :: Options -> String
absFile       = mkFile withLang "Abs" "hs"
absFileM      = mkMod  withLang "Abs" 
alexFile      = mkFile withLang "Lex" "x"
alexFileM     = mkMod  withLang "Lex"
composOpFile  = mkFile noLang   "ComposOp" "hs"
composOpFileM = mkMod noLang    "ComposOp"
happyFile     = mkFile withLang "Par" "y"
happyFileM    = mkMod  withLang "Par"
latexFile     = mkFile withLang "Doc" "tex"
templateFile  = mkFile withLang "Skel" "hs"
templateFileM = mkMod  withLang "Skel"
printerFile   = mkFile withLang "Print" "hs"
printerFileM  = mkMod  withLang "Print"
dviFile       = mkFile withLang "Doc" "dvi"
psFile        = mkFile withLang "Doc" "ps"
gfAbs         = mkFile withLang "" "Abs.gf"
gfConc        = mkFile withLang "" "Conc.gf"
tFile         = mkFile withLang "Test" "hs"
tFileM        = mkMod  withLang "Test"
errFile       = mkFile noLang   "ErrM" "hs"
errFileM      = mkMod  noLang   "ErrM"
shareFile     = mkFile noLang   "SharedString" "hs"
shareFileM    = mkMod  noLang   "SharedString"
layoutFileM   = mkMod  withLang "Layout"
xmlFileM      = mkMod  withLang "XML"
layoutFile    = mkFile withLang "Layout" "hs"

data Options = Options 
    { 
     make :: Bool,
     alexMode :: AlexMode,
     inDir :: Bool,
     shareStrings :: Bool,
     byteStrings :: Bool,
     glr :: HappyMode,
     xml :: Int,
     inPackage :: Maybe String,
     lang :: String
    }

makeAllGADT :: Bool -> AlexMode -> Bool -> Bool -> Bool -> Bool -> Int 
	   -> Maybe String -- ^ The hierarchical package to put the modules
	                   --   in, or Nothing.
	   -> String -> FilePath -> IO ()
makeAllGADT m am d ss bs g x p n file = do
  let opts = Options { make = m, alexMode = am, inDir = d, shareStrings = ss, byteStrings = bs,
 		       glr = if g then GLR else Standard, xml = x, 
 		       inPackage = p, lang = n }        
      absMod = absFileM opts
      composOpMod = composOpFileM opts
      lexMod = alexFileM opts
      parMod = happyFileM opts
      prMod  = printerFileM opts
      layMod = layoutFileM opts
      errMod = errFileM opts
      shareMod = shareFileM opts
  (cf, isOK) <- tryReadCF file
  if isOK then do
    let dir = codeDir opts
    when (not (null dir)) $ do
			    putStrLn $ "Creating directory " ++ dir
			    prepareDir dir
    writeFileRep (absFile opts) $ cf2Abstract (byteStrings opts) absMod cf composOpMod
    writeFileRep (composOpFile opts) $ composOp composOpMod
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
    writeFileRep (templateFile opts) $ cf2Template (templateFileM opts) absMod errMod cf
    writeFileRep (printerFile opts)  $ cf2Printer prMod absMod cf
    when (hasLayout cf) $ writeFileRep (layoutFile opts) $ cf2Layout (alexMode opts == Alex1) (inDir opts) layMod lexMod cf
    writeFileRep (tFile opts)        $ testfile opts cf
    writeFileRep (errFile opts)      $ errM errMod cf
    when (shareStrings opts) $ writeFileRep (shareFile opts)    $ sharedString shareMod (byteStrings opts) cf
    when (make opts) $ writeFileRep "Makefile" $ makefile opts
    case xml opts of
      2 -> makeXML (lang opts) True cf
      1 -> makeXML (lang opts) False cf
      _ -> return ()
    putStrLn $ "Done!"
   else do putStrLn $ "Failed!"
	   exitFailure

pkgToDir :: String -> FilePath
pkgToDir s = replace '.' pathSep s

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
  makeA = unlines 
                [
 		 "all:", 
                 "\thappy -gca " ++ glr_params ++ happyFile opts, 
		 "\talex -g "  ++ alexFile opts,
		 "\t" ++ cd ("latex " ++ basename (latexFile opts)
			     ++ "; " ++ "dvips " ++ basename (dviFile opts) 
			     ++ " -o " ++ basename (psFile opts)),
		 "\tghc --make " ++ tFile opts ++ " -o " ++ mkFile withLang "Test" "" opts,
		 "clean:",
		 "\t-rm -f " ++ unwords (map (dir++) [
						       "*.log", "*.aux", "*.hi", 
						       "*.o", "*.dvi"
						      ]),
		 "\t-rm -f " ++ psFile opts,

		 "distclean: clean",
		 "\t-rm -f " ++ unwords [
					 mkFile withLang "Doc" "*" opts,
					 mkFile withLang "Lex" "*" opts,
					 mkFile withLang "Par" "*" opts,
					 mkFile withLang "Layout" "*" opts,
					 mkFile withLang "Skel" "*" opts,
					 mkFile withLang "Print" "*" opts,
					 mkFile withLang "Test" "*" opts,
					 mkFile withLang "Abs" "*" opts,
					 mkFile withLang "ComposOp" "*" opts,
					 mkFile withLang "Test" "" opts,
					 mkFile noLang   "ErrM" "*" opts,
					 mkFile noLang   "SharedString" "*" opts,
                                         dir ++ lang opts ++ ".dtd",
					 mkFile withLang "XML" "*" opts, 
					 "Makefile*"
					],
		 if null dir then "" else "\t-rmdir -p " ++ dir
		]


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

composOp :: String -> String
composOp composOpMod = unlines
    [
     "{-# OPTIONS_GHC -fglasgow-exts #-}",
     "module " ++ composOpMod ++ " (Compos(..),composOp,composOpM,composOpM_,composOpMonoid,",
     "                 composOpMPlus,composOpFold) where",
     "",
     "import Control.Monad.Identity",
     "import Data.Monoid",
     "",
     "class Compos t where",
     "  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)",
     "         -> (forall a. t a -> m (t a)) -> t c -> m (t c)",
     "",
     "composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c",
     "composOp f = runIdentity . composOpM (Identity . f)",
     "",
     "composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)",
     "composOpM = compos return ap",
     "",
     "composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()",
     "composOpM_ = composOpFold (return ()) (>>)",
     "",
     "composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m",
     "composOpMonoid = composOpFold mempty mappend",
     "",
     "composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b",
     "composOpMPlus = composOpFold mzero mplus",
     "",
     "composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b",
     "composOpFold z c f = unC . compos (\\_ -> C z) (\\(C x) (C y) -> C (c x y)) (C . f)",
     "",
     "newtype C b a = C { unC :: b }"
    ]
