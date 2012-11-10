{-
    BNF Converter: OCaml main file
    Copyright (C) 2005  Author:  Kristofer Johannisson

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

-- based on BNFC Haskell backend


module OCamlTop (makeOCaml) where 

import CF
import CFtoOCamlYacc
import CFtoOCamlLex
import CFtoLatex
import CFtoOCamlAbs
import CFtoOCamlTemplate
import CFtoOCamlPrinter
import CFtoOCamlShow
import CFtoOCamlTest
import CFtoXML
import GetCF
import Utils
import Options

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

absFile, absFileM, ocamllexFile, ocamllexFileM, dviFile,
 ocamlyaccFile, ocamlyaccFileM,
 latexFile, utilFile, utilFileM,
 templateFile, templateFileM, 
 printerFile, printerFileM,
 psFile, tFile, tFileM :: Options -> String
absFile       = mkFile withLang "Abs" "ml"
absFileM      = mkMod  withLang "Abs" 
ocamllexFile      = mkFile withLang "Lex" "mll"
ocamllexFileM     = mkMod  withLang "Lex"
ocamlyaccFile     = mkFile withLang "Par" "mly"
ocamlyaccFileM    = mkMod  withLang "Par"
latexFile     = mkFile withLang "Doc" "tex"
templateFile  = mkFile withLang "Skel" "ml"
templateFileM = mkMod  withLang "Skel"
printerFile   = mkFile withLang "Print" "ml"
printerFileM  = mkMod  withLang "Print"
showFile      = mkFile  withLang "Show" "ml"
showFileM     = mkMod  withLang "Show"
dviFile       = mkFile withLang "Doc" "dvi"
psFile        = mkFile withLang "Doc" "ps"
tFile         = mkFile withLang "Test" "ml"
tFileM        = mkMod  withLang "Test"
utilFile       = mkFile noLang   "BNFC_Util" "ml"
utilFileM      = mkMod  noLang   "BNFC_Util"
xmlFileM      = mkMod  withLang "XML"

type Options = SharedOptions

-- FIXME: we probably don't need all these arguments
makeOCaml :: Options -> FilePath -> IO ()
makeOCaml opts file = do
  let absMod = absFileM opts
      lexMod = ocamllexFileM opts
      parMod = ocamlyaccFileM opts
      prMod  = printerFileM opts
      showMod = showFileM opts
--      layMod = layoutFileM opts
      utilMod = utilFileM opts
  (cf, isOK) <- tryReadCF [formatOptOCAML] file
  if isOK then do
    let dir = codeDir opts
    when (not (null dir)) $ do
                            putStrLn $ "Creating directory " ++ dir
                            prepareDir dir
    writeFileRep (absFile opts) $ cf2Abstract absMod cf
    writeFileRep (ocamllexFile opts) $ cf2ocamllex lexMod parMod cf
    writeFileRep (ocamlyaccFile opts) $ 
                 cf2ocamlyacc parMod absMod lexMod  cf
    writeFileRep (latexFile opts)    $ cfToLatex (lang opts) cf
    writeFileRep (templateFile opts) $ cf2Template (templateFileM opts) absMod cf
    writeFileRep (printerFile opts)  $ cf2Printer prMod absMod cf
    writeFileRep (showFile opts)  $ cf2show showMod absMod cf
    writeFileRep (tFile opts)        $ ocamlTestfile absMod lexMod parMod prMod showMod cf
    writeFileRep (utilFile opts)      $ utilM 
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
  dir = let d = codeDir opts in if null d then "" else d ++ [pathSep]
  cd c = if null dir then c else "(cd " ++ dir ++ "; " ++ c ++ ")"
  makeA = unlines 
                [
                 "all:",
                 "\tocamlyacc " ++ ocamlyaccFile opts, 
                 "\tocamllex "  ++ ocamllexFile opts,
                 "\t" ++ cd ("latex " ++ basename (latexFile opts)
                             ++ "; " ++ "dvips " ++ basename (dviFile opts) 
                             ++ " -o " ++ basename (psFile opts)),
                 "\tocamlc -o " ++ mkFile withLang "Test" "" opts +++
                    utilFile opts +++
                    absFile opts +++ templateFile opts +++
                    showFile opts +++ printerFile opts +++
                    mkFile withLang "Par" "mli" opts +++
                    mkFile withLang "Par" "ml" opts +++
                    mkFile withLang "Lex" "ml" opts +++
                    tFile opts, 
                 "",
                 "clean:",
                 "\t-rm -f " ++ unwords (map (dir++) [
                                                       "*.log", "*.aux", "*.cmi", 
                                                       "*.cmo", "*.o", "*.dvi"
                                                      ]),
                 "\t-rm -f " ++ psFile opts,
                 "",
                 "distclean: clean",
                 "\t-rm -f " ++ unwords [
                                         mkFile withLang "Doc" "*" opts,
                                         mkFile withLang "Lex" "*" opts,
                                         mkFile withLang "Par" "*" opts,
                                         mkFile withLang "Layout" "*" opts,
                                         mkFile withLang "Skel" "*" opts,
                                         mkFile withLang "Print" "*" opts,
                                         mkFile withLang "Show" "*" opts,
                                         mkFile withLang "Test" "*" opts,
                                         mkFile withLang "Abs" "*" opts,
                                         mkFile withLang "Test" "" opts,
                                         utilFile opts,
                                         "Makefile*"
                                        ]
                ]


utilM :: String
utilM = unlines 
    ["(* automatically generated by BNFC *)",
     "",
     "open Lexing",
     "",
     "(* this should really be in the parser, but ocamlyacc won't put it in the .mli *)",
     "exception Parse_error of Lexing.position * Lexing.position"
    ]

