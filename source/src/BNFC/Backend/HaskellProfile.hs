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

module BNFC.Backend.HaskellProfile (makeHaskellProfile) where

import Control.Monad (when)
import Data.Maybe (isJust)

import BNFC.CF
import BNFC.Options hiding (Backend)
import BNFC.Backend.Base
import BNFC.Backend.HaskellProfile.CFtoHappyProfile
import BNFC.Backend.Haskell.CFtoAlex
import BNFC.Backend.Haskell.CFtoAlex2
import BNFC.Backend.Haskell.MkErrM

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

absFileM, alexFile, alexFileM, happyFile, happyFileM, errFile, errFileM, tFile,
  mFile :: Bool -> String -> FilePath
absFileM      = nameMod  "Abs"
alexFile      = nameFile "Lex" "x"
alexFileM     = nameMod  "Lex"
happyFile     = nameFile "Par" "y"
happyFileM    = nameMod  "Par"
tFile         = nameFile "Test" "hs"
mFile inDir n = if inDir then n ++ "/" ++ "Makefile" else "Makefile"
errFile b n   = if b then n ++ "/" ++ "ErrM.hs" else "ErrM.hs"
errFileM b n  = if b then n ++ "." ++ "ErrM" else "ErrM"

makeHaskellProfile :: SharedOptions -> CFP -> Backend
makeHaskellProfile opts cfp = do
  let absMod = absFileM      (inDir opts) name
      lexMod = alexFileM     (inDir opts) name
      parMod = happyFileM    (inDir opts) name
      errMod = errFileM      (inDir opts) name
  let cf = cfp2cf cfp
  do
----    mkfile (absFile  (inDir opts) name) $ cf2Abstract (absFileM (inDir opts) name) cf
    if alexMode opts == Alex1 then do
                    mkfile (alexFile (inDir opts) name) $ cf2alex lexMod errMod cf
                    liftIO $ putStrLn "   (Use Alex 1.1 to compile.)"
               else do
                    mkfile (alexFile (inDir opts) name) $ cf2alex2 lexMod errMod "" False False cf
                    liftIO $ putStrLn "   (Use Alex 2.0 to compile.)"
    mkfile (happyFile (inDir opts) name) $
                 cf2HappyProfileS parMod absMod lexMod errMod cfp
    liftIO $ putStrLn "   (Tested with Happy 1.13)"
----    mkfile (templateFile (inDir opts) name) $
----             cf2Template tplMod absMod errMod cf
----    mkfile (printerFile (inDir opts) name)  $ cf2Printer prMod absMod cf
----    if hasLayout cf then
----      mkfile (layoutFile (inDir opts) name) $ cf2Layout alex1 (inDir opts) layMod lexMod cf
----      else return ()
    mkfile (tFile (inDir opts) name)        $ testfile (inDir opts) name (xml opts>0) cf
    mkfile (errFile (inDir opts) name) $ mkErrM errMod (ghcExtensions opts)
    when (isJust $ make opts)
        (mkfile (mFile (inDir opts) name) $ makefile (inDir opts) name)
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
                 '\t' : if inDir then
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
----                                       "Layout" ++ name ++ ".*",
----                                       "Skel" ++ name ++ ".*",
----                                       "Print" ++ name ++ ".*",
                                           "Test" ++ name ++ ".*",
----                                       "Abs" ++ name ++ ".*",
                                           "Test" ++ name,
                                           "ErrM.*",
----                                       name ++ ".dtd",
----                                       "XML" ++ name ++ ".*",
                                           "Makefile*"
                                          ]
                ]


testfile :: Bool -> String -> Bool -> CF -> String
testfile inDir name _ cf = makeA where

 makeA = let lay = hasLayout cf
         in unlines
                ["-- automatically generated by BNF Converter",
                 "module Main where\n",
                 "",
                 "import Trees",
                 "import Profile",
                 "import System.IO ( stdin, hGetContents )",
                 "import System.Environment ( getArgs, getProgName )",
                 "import System.Exit ( exitFailure )",
                 "",
                 "import " ++ alexFileM     inDir name,
                 "import " ++ happyFileM    inDir name,
----             "import " ++ templateFileM inDir name,
----             "import " ++ printerFileM  inDir name,
----             "import " ++ absFileM      inDir name,
----             if lay then ("import " ++ layoutFileM inDir name) else "",
----             if xml then ("import " ++ xmlFileM inDir name) else "",
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
                 "usage :: IO ()",
                 "usage = do",
                 "  putStrLn $ unlines",
                 "    [ \"usage: Call with one of the following argument combinations:\"",
                 "    , \"  --help          Display this help message.\"",
                 "    , \"  (no arguments)  Parse stdin.\"",
                 "    , \"  (file)          Parse content of file.\"",
                 "    ]",
                 "  exitFailure",
                 "",
                 "main :: IO ()",
                 "main = do",
                 "  args <- getArgs",
                 "  case args of",
                 "    [\"--help\"] -> usage",
                 "    []  -> hGetContents stdin >>= run " ++ firstParser,
                 "    [f] -> runFile " ++ firstParser ++ " f",
                 "    _   -> do progName <- getProgName",
                 "              putStrLn $ progName ++ \": excess arguments.\""
                 ]
                  where firstParser = 'p' : show (firstEntry cf)
