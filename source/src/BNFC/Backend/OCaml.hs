{-
    BNF Converter: OCaml main file
    Copyright (C) 2005  Author:  Kristofer Johannisson

-}

-- based on BNFC Haskell backend


module BNFC.Backend.OCaml (makeOCaml) where

import System.FilePath (pathSeparator, (</>))

import BNFC.Backend.Base                    (MkFiles, mkfile)
import BNFC.Backend.Common.Makefile
import BNFC.Backend.OCaml.CFtoOCamlAbs
import BNFC.Backend.OCaml.CFtoOCamlLex
import BNFC.Backend.OCaml.CFtoOCamlPrinter
import BNFC.Backend.OCaml.CFtoOCamlShow
import BNFC.Backend.OCaml.CFtoOCamlTemplate
import BNFC.Backend.OCaml.CFtoOCamlTest     (ocamlTestfile)
import BNFC.Backend.OCaml.CFtoOCamlYacc
import BNFC.Backend.XML                     (makeXML)
import BNFC.CF
import BNFC.Options
import BNFC.PrettyPrint
import BNFC.Utils

import qualified BNFC.Backend.C as C

-- naming conventions

noLang :: SharedOptions -> String -> String
noLang _ name = name

withLang :: SharedOptions -> String -> String
withLang opts name = name ++ sanitizedLang opts

mkMod :: (SharedOptions -> String -> String) -> String -> SharedOptions -> String
mkMod addLang name opts =
    pref ++ if inDir opts then sanitizedLang opts ++ "." ++ name else addLang opts name
        where pref = maybe "" (++".") (inPackage opts)

mkFile :: (SharedOptions -> String -> String) -> String -> String -> SharedOptions -> FilePath
mkFile addLang name ext opts =
    pref ++ if inDir opts
       then sanitizedLang opts </> name ++ ext'
       else addLang opts name ++ if null ext then "" else ext'
    where pref = maybe "" (\ p -> pkgToDir p </> "") (inPackage opts)
          ext' = if null ext then "" else "." ++ ext

-- | Turn language name into a valid ocaml module identifier.
sanitizedLang :: SharedOptions -> String
sanitizedLang = camelCase_ . lang


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
    mkfile (absFile opts)       comment $ cf2Abstract absMod cf
    mkfile (ocamllexFile opts)  comment $ cf2ocamllex lexMod parMod cf
    mkfile (ocamlyaccFile opts) C.comment $
                 cf2ocamlyacc parMod absMod lexMod  cf
    mkfile (templateFile opts)  comment $ cf2Template (templateFileM opts) absMod cf
    mkfile (printerFile opts)   comment $ cf2Printer prMod absMod cf
    mkfile (showFile opts)      comment $ cf2show showMod absMod cf
    mkfile (tFile opts)         comment $ ocamlTestfile absMod lexMod parMod prMod showMod cf
    mkfile (utilFile opts)      comment $ utilM
    mkMakefile opts $ makefile opts
    case xml opts of
      2 -> makeXML opts True cf
      1 -> makeXML opts False cf
      _ -> return ()

comment :: String -> String
comment x = unwords [ "(*", x, "*)" ]

pkgToDir :: String -> FilePath
pkgToDir = replace '.' pathSeparator

codeDir :: SharedOptions -> FilePath
codeDir opts = let pref = maybe "" pkgToDir (inPackage opts)
                   dir = if inDir opts then sanitizedLang opts else ""
                   sep = if null pref || null dir then "" else [pathSeparator]
                 in pref ++ sep ++ dir

makefile :: SharedOptions -> String -> Doc
makefile opts basename = vcat
    [ mkVar "OCAMLC" "ocamlc"
    , mkVar "OCAMLYACC" $ case ocamlParser opts of
        OCamlYacc -> "ocamlyacc"
        Menhir    -> "menhir"
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
                                 basename ]]
    ]
  where dir = let d = codeDir opts in if null d then "" else d ++ [pathSeparator]

utilM :: String
utilM = unlines
    ["open Lexing",
     "",
     "(* this should really be in the parser, but ocamlyacc won't put it in the .mli *)",
     "exception Parse_error of Lexing.position * Lexing.position"
    ]
