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

module BNFC.Backend.HaskellProfile (makeHaskellProfile) where

-- import Utils
import BNFC.CF
import BNFC.Options
import BNFC.Backend.HaskellProfile.CFtoHappyProfile
import BNFC.Backend.Haskell.CFtoAlex
import BNFC.Backend.Haskell.CFtoAlex2
import BNFC.Backend.Haskell.MkErrM
import BNFC.Utils

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

absFile, absFileM, alexFile, alexFileM, gfAbs, gfConc, happyFile, happyFileM,
 errFile, errFileM, templateFile, templateFileM, printerFile, printerFileM,
 layoutFile, layoutFileM, tFile, tFileM, mFile :: Bool -> String -> FilePath
absFile       = nameFile "Abs" "hs"
absFileM      = nameMod  "Abs"
alexFile      = nameFile "Lex" "x"
alexFileM     = nameMod  "Lex"
happyFile     = nameFile "Par" "y"
happyFileM    = nameMod  "Par"
templateFile  = nameFile "Skel" "hs"
templateFileM = nameMod  "Skel"
printerFile   = nameFile "Print" "hs"
printerFileM  = nameMod  "Print"
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

makeHaskellProfile :: SharedOptions -> CFP -> IO ()
makeHaskellProfile opts cfp = do
  let absMod = absFileM      (inDir opts) name
      lexMod = alexFileM     (inDir opts) name
      parMod = happyFileM    (inDir opts) name
      prMod  = printerFileM  (inDir opts) name
      layMod = layoutFileM   (inDir opts) name
      tplMod = templateFileM (inDir opts) name
      errMod = errFileM      (inDir opts) name
  let cf = cfp2cf cfp
  do
    when (inDir opts) (prepareDir name)
----    writeFileRep (absFile  (inDir opts) name) $ cf2Abstract (absFileM (inDir opts) name) cf
    if (alexMode opts == Alex1) then do
		    writeFileRep (alexFile (inDir opts) name) $ cf2alex lexMod errMod cf
		    putStrLn "   (Use Alex 1.1 to compile.)"
	       else do
		    writeFileRep (alexFile (inDir opts) name) $ cf2alex2 lexMod errMod "" False False cf
                    putStrLn "   (Use Alex 2.0 to compile.)"
    writeFileRep (happyFile (inDir opts) name) $
		 cf2HappyProfileS parMod absMod lexMod errMod cfp
    putStrLn "   (Tested with Happy 1.13)"
----    writeFileRep (templateFile (inDir opts) name) $
----		 cf2Template tplMod absMod errMod cf
----    writeFileRep (printerFile (inDir opts) name)  $ cf2Printer prMod absMod cf
----    if hasLayout cf then
----      writeFileRep (layoutFile (inDir opts) name) $ cf2Layout alex1 (inDir opts) layMod lexMod cf
----      else return ()
    writeFileRep (tFile (inDir opts) name)        $ testfile (inDir opts) name (xml opts>0) cf
    writeFileRep (errFile (inDir opts) name)      $ errM errMod cf
    if (make opts)
       then (writeFileRep (mFile (inDir opts) name) $ makefile (inDir opts) name)
       else return ()
----    case xml of
----      2 -> makeXML name True cf
----      1 -> makeXML name False cf
----      _ -> return ()
  where name = lang opts

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
		 "\t" ++ if inDir then
		           "(" ++ "cd ..; " ++ ghcCommand ++ ")"
                         else ghcCommand,
		 "clean:",
		 "\t rm -f *.hi *.o",
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
