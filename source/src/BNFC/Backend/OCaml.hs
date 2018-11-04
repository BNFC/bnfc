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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

-- based on BNFC Haskell backend


module BNFC.Backend.OCaml (makeOCaml) where

import System.FilePath (pathSeparator, (</>))

import BNFC.Backend.Base hiding (Backend)
import BNFC.Backend.Common.Makefile
import BNFC.Backend.OCaml.CFtoOCamlAbs
import BNFC.Backend.OCaml.CFtoOCamlLex
import BNFC.Backend.OCaml.CFtoOCamlPrinter
import BNFC.Backend.OCaml.CFtoOCamlShow
import BNFC.Backend.OCaml.CFtoOCamlTemplate
import BNFC.Backend.OCaml.CFtoOCamlTest
import BNFC.Backend.OCaml.CFtoOCamlYacc
import BNFC.Backend.XML
import BNFC.CF
import BNFC.Options
import BNFC.PrettyPrint
import BNFC.Utils

-- naming conventions

noLang :: SharedOptions -> String -> String
noLang _ name = name

withLang :: SharedOptions -> String -> String
withLang opts name = name ++ lang opts

mkMod :: (SharedOptions -> String -> String) -> String -> SharedOptions -> String
mkMod addLang name opts =
    pref ++ if inDir opts then lang opts ++ "." ++ name else addLang opts name
        where pref = maybe "" (++".") (inPackage opts)

mkFile :: (SharedOptions -> String -> String) -> String -> String -> SharedOptions -> FilePath
mkFile addLang name ext opts =
    pref ++ if inDir opts
       then lang opts </> name ++ ext'
       else addLang opts name ++ if null ext then "" else ext'
    where pref = maybe "" (\p->pkgToDir p </> "") (inPackage opts)
          ext' = if null ext then "" else "." ++ ext

absFile, absFileM, ocamllexFile, ocamllexFileM, ocamlyaccFile, ocamlyaccFileM,
  utilFile, templateFile, templateFileM, printerFile, printerFileM,
  tFile :: SharedOptions -> String
absFile       = mkFile withLang "Abs" "ml"
absFileM      = mkMod  withLang "Abs"
ocamllexFile      = mkFile withLang "Lex" "mll"
ocamllexFileM     = mkMod  withLang "Lex"
ocamlyaccFile     = mkFile withLang "Par" "mly"
ocamlyaccFileM    = mkMod  withLang "Par"
templateFile  = mkFile withLang "Skel" "ml"
templateFileM = mkMod  withLang "Skel"
printerFile   = mkFile withLang "Print" "ml"
printerFileM  = mkMod  withLang "Print"
showFile      = mkFile  withLang "Show" "ml"
showFileM     = mkMod  withLang "Show"
tFile         = mkFile withLang "Test" "ml"
utilFile       = mkFile noLang   "BNFC_Util" "ml"

makeOCaml :: SharedOptions -> CF -> MkFiles ()
makeOCaml opts cf = do
  let absMod = absFileM opts
      lexMod = ocamllexFileM opts
      parMod = ocamlyaccFileM opts
      prMod  = printerFileM opts
      showMod = showFileM opts
  do
    mkfile (absFile opts) $ cf2Abstract absMod cf
    mkfile (ocamllexFile opts) $ cf2ocamllex lexMod parMod cf
    mkfile (ocamlyaccFile opts) $
                 cf2ocamlyacc parMod absMod lexMod  cf
    mkfile (templateFile opts) $ cf2Template (templateFileM opts) absMod cf
    mkfile (printerFile opts)  $ cf2Printer prMod absMod cf
    mkfile (showFile opts)  $ cf2show showMod absMod cf
    mkfile (tFile opts) $ ocamlTestfile absMod lexMod parMod prMod showMod cf
    mkfile (utilFile opts) utilM
    mkMakefile opts $ makefile opts
    case xml opts of
      2 -> makeXML opts True cf
      1 -> makeXML opts False cf
      _ -> return ()

pkgToDir :: String -> FilePath
pkgToDir = replace '.' pathSeparator

codeDir :: SharedOptions -> FilePath
codeDir opts = let pref = maybe "" pkgToDir (inPackage opts)
                   dir = if inDir opts then lang opts else ""
                   sep = if null pref || null dir then "" else [pathSeparator]
                 in pref ++ sep ++ dir

makefile :: SharedOptions -> Doc
makefile opts = vcat
    [ mkVar "OCAMLC" "ocamlc"
    , mkVar "OCAMLYACC" "ocamlyacc"
    , mkVar "OCAMLLEX" "ocamllex"
    , mkVar "OCAMLCFLAGS" ""
    , mkRule "all" []
        [ "$(OCAMLYACC) " ++ ocamlyaccFile opts
        , "$(OCAMLLEX) "  ++ ocamllexFile opts
        , "$(OCAMLC) $(OCAMLCFLAGS) -o " ++ mkFile withLang "Test" "" opts +++
                          utilFile opts +++
                          absFile opts +++ templateFile opts +++
                          showFile opts +++ printerFile opts +++
                          mkFile withLang "Par" "mli" opts +++
                          mkFile withLang "Par" "ml" opts +++
                          mkFile withLang "Lex" "ml" opts +++
                          tFile opts ]
    , mkRule "clean" []
        [ "-rm -f " ++ unwords (map (dir++) [ "*.cmi", "*.cmo", "*.o" ]) ]
    , mkRule "distclean" ["clean"]
        [ "-rm -f " ++ unwords [ mkFile withLang "Lex" "*" opts,
                                 mkFile withLang "Par" "*" opts,
                                 mkFile withLang "Layout" "*" opts,
                                 mkFile withLang "Skel" "*" opts,
                                 mkFile withLang "Print" "*" opts,
                                 mkFile withLang "Show" "*" opts,
                                 mkFile withLang "Test" "*" opts,
                                 mkFile withLang "Abs" "*" opts,
                                 mkFile withLang "Test" "" opts,
                                 utilFile opts,
                                 "Makefile*" ]]
    ]
  where dir = let d = codeDir opts in if null d then "" else d ++ [pathSeparator]

utilM :: String
utilM = unlines
    ["(* automatically generated by BNFC *)",
     "",
     "open Lexing",
     "",
     "(* this should really be in the parser, but ocamlyacc won't put it in the .mli *)",
     "exception Parse_error of Lexing.position * Lexing.position"
    ]
