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

module ProfileTop (makeAllProfile) where 

-- import Utils
import CF
import CFtoHappyProfile
import CFtoAlex
import CFtoAlex2
import CFtoLatex
import MkErrM
---- import CFtoAbstract
---- import CFtoTemplate
---- import CFtoPrinter
---- import CFtoLayout
---- import CFtoXML
-- import CFtoGF		( cf2AbsGF, cf2ConcGF )
import GetCF
import Utils

import Data.Char
import System.Exit(exitFailure)
import Control.Monad(when)

-- naming conventions

nameMod :: String -> Bool -> String -> FilePath
nameMod name inDir lang =
    if inDir 
       then lang ++ "." ++ name 
       else name ++ lang 

nameFile :: String -> String -> Bool -> String -> FilePath
nameFile name ext inDir lang = 
    if inDir 
       then lang ++ "/" ++ name ++ "." ++ ext
       else name ++ lang ++ "." ++ ext

absFile, absFileM, alexFile, alexFileM, dviFile,
 gfAbs, gfConc,
 happyFile, happyFileM,
 latexFile, errFile, errFileM,
 templateFile, templateFileM, 
 printerFile, printerFileM,
 layoutFile, layoutFileM, 
 psFile, tFile, tFileM, mFile :: Bool -> String -> FilePath
absFile       = nameFile "Abs" "hs"
absFileM      = nameMod  "Abs" 
alexFile      = nameFile "Lex" "x"
alexFileM     = nameMod  "Lex"
happyFile     = nameFile "Par" "y"
happyFileM    = nameMod  "Par"
latexFile     = nameFile "Doc" "tex"
templateFile  = nameFile "Skel" "hs"
templateFileM = nameMod  "Skel"
printerFile   = nameFile "Print" "hs"
printerFileM  = nameMod  "Print"
dviFile       = nameFile "Doc" "dvi"
psFile        = nameFile "Doc" "ps"
gfAbs         = nameFile "" "Abs.gf"
gfConc        = nameFile "" "Conc.gf"
tFile         = nameFile "Test" "hs"
tFileM        = nameMod  "Test"
mFile inDir n = if inDir then n ++ "/" ++ "Makefile" else "Makefile"
errFile b n   = if b then n ++ "/" ++ "ErrM.hs" else "ErrM.hs"
errFileM b n  = if b then n ++ "." ++ "ErrM" else "ErrM"
layoutFileM   = nameMod  "Layout"
xmlFileM      = nameMod  "XML"
layoutFile    = nameFile "Layout" "hs"

makeAllProfile :: Bool -> Bool -> Bool -> Int -> String -> FilePath -> IO ()
makeAllProfile make alex1 inDir xml name file = do
  let absMod = absFileM      inDir name
      lexMod = alexFileM     inDir name
      parMod = happyFileM    inDir name
      prMod  = printerFileM  inDir name
      layMod = layoutFileM   inDir name
      tplMod = templateFileM inDir name
      errMod = errFileM      inDir name
  (cfp, isOK) <- tryReadCFP file
  let cf = cfp2cf cfp
  if isOK then do

    when inDir (prepareDir name)
----    writeFileRep (absFile  inDir name) $ cf2Abstract (absFileM inDir name) cf
    if (alex1) then do
		    writeFileRep (alexFile inDir name) $ cf2alex lexMod errMod cf
		    putStrLn "   (Use Alex 1.1 to compile.)" 
	       else do
		    writeFileRep (alexFile inDir name) $ cf2alex2 lexMod errMod "" False False cf
                    putStrLn "   (Use Alex 2.0 to compile.)"
    writeFileRep (happyFile inDir name) $ 
		 cf2HappyProfileS parMod absMod lexMod errMod cfp
    putStrLn "   (Tested with Happy 1.13)"
    writeFileRep (latexFile inDir name)    $ cfToLatex name cf
----    writeFileRep (templateFile inDir name) $ 
----		 cf2Template tplMod absMod errMod cf
----    writeFileRep (printerFile inDir name)  $ cf2Printer prMod absMod cf
----    if hasLayout cf then 
----      writeFileRep (layoutFile inDir name) $ cf2Layout alex1 inDir layMod lexMod cf
----      else return ()
    writeFileRep (tFile inDir name)        $ testfile inDir name (xml>0) cf
    writeFileRep (errFile inDir name)      $ errM errMod cf
    if make 
       then (writeFileRep (mFile inDir name) $ makefile inDir name) 
       else return () 
----    case xml of
----      2 -> makeXML name True cf
----      1 -> makeXML name False cf
----      _ -> return ()
----    putStrLn $ "Done!"
   else do putStrLn $ "Failed!"
	   exitFailure

makefile :: Bool -> String -> String
makefile inDir name = makeA where
  name' = if inDir then "" else name -- Makefile is inDir
  ghcCommand = "ghc --make "++ tFile inDir name ++ " -o " ++ 
                      if inDir then name ++ "/" ++ "Test" else "Test" ++ name
  makeA = unlines 
                [
 		 "all:", 
                 "\thappy -gca " ++ happyFile False name', 
		 "\talex "  ++ alexFile  False name',
                 "\tlatex " ++ latexFile False name',
		 "\tdvips " ++ dviFile   False name' ++ " -o " ++ psFile False name',
		 "\t" ++ if inDir then 
		           "(" ++ "cd ..; " ++ ghcCommand ++ ")"
                         else ghcCommand,
		 "clean:",
		 "\t rm -f " ++ unwords [
                                         "*.log *.aux *.hi *.o *.dvi",
				         psFile False name',
				         "*.o"
                                        ],
		 "distclean: " ++ if inDir then "" else "clean",
		 if inDir then
		   "\t rm -rf ../" ++ name -- erase this directory!
		 else
		   "\t rm -f " ++ unwords [
					   "Doc" ++ name ++ ".*",
					   "Lex" ++ name ++ ".*",
					   "Par" ++ name ++ ".*",
----					   "Layout" ++ name ++ ".*",
----					   "Skel" ++ name ++ ".*",
----					   "Print" ++ name ++ ".*",
					   "Test" ++ name ++ ".*",
----					   "Abs" ++ name ++ ".*", 
					   "Test" ++ name,
					   "ErrM.*",
----                                       name ++ ".dtd",
----					   "XML" ++ name ++ ".*", 
					   "Makefile*"
					  ]
		]


testfile :: Bool -> String -> Bool -> CF -> String
testfile inDir name xml cf = makeA where

 makeA = let lay = hasLayout cf 
             xpr = if xml then "XPrint a, " else ""
         in unlines
	        ["-- automatically generated by BNF Converter",
		 "module Main where\n",
	         "",
                 "import Trees",
                 "import Profile",
	         "import System.IO ( stdin, hGetContents )",
	         "import System.Environment ( getArgs, getProgName )",
		 "",
		 "import " ++ alexFileM     inDir name,
		 "import " ++ happyFileM    inDir name,
----		 "import " ++ templateFileM inDir name,
----	         "import " ++ printerFileM  inDir name,
----	         "import " ++ absFileM      inDir name,
----	         if lay then ("import " ++ layoutFileM inDir name) else "",
----	         if xml then ("import " ++ xmlFileM inDir name) else "",
	         "import " ++ errFileM      inDir name,
		 "",
		 "type ParseFun = [Token] -> Err CFTree",
	         "",
                 "myLLexer = " ++ if lay then "resolveLayout True . myLexer" 
                                         else "myLexer",
                 "",
		 "runFile :: ParseFun -> FilePath -> IO ()",
		 "runFile p f = readFile f >>= run p",
		 "",
		 "run :: ParseFun -> String -> IO ()",
  "run p s = do",
  "  let ts = myLLexer s",
  "  let etree = p ts",
  "  case etree of",
  "    Ok tree -> do",
  "      case postParse tree of",
  "        Bad s    -> do",  
  "          putStrLn \"\\nParse Failed... CFTree:\\n\"",
  "          putStrLn $ prCFTree tree",
  "          putStrLn s",
  "        Ok  tree -> do", 
  "          putStrLn \"\\nParse Successful!\"",
  "          putStrLn $ \"\\n[Abstract Syntax]\\n\\n\" ++ prt tree",
  "    Bad s -> do",
  "      putStrLn s",
  "      putStrLn \"\\nParse failed... tokenization:\"",
  "      print ts",
		 "",
		 "main :: IO ()",
		 "main = do args <- getArgs",
		 "          case args of",
		 "            []  -> hGetContents stdin >>= run " ++ firstParser,
		 "            [f] -> runFile " ++ firstParser ++ " f",
		 "            _   -> do progName <- getProgName",
		 "                      putStrLn $ progName ++ \": excess arguments.\""
		 ]
		  where firstParser = 'p' : firstEntry cf
