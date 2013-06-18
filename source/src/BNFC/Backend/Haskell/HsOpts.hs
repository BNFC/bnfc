module BNFC.Backend.Haskell.HsOpts  where

import BNFC.Utils
import BNFC.Options
import System.FilePath (pathSeparator)

type Options = SharedOptions

alex1 opts = alexMode opts == Alex1

absFile, absFileM, alexFile, alexFileM, dviFile,
 composOpFile, composOpFileM,
 gfAbs, gfConc,
 happyFile, happyFileM,
 latexFile, errFile, errFileM,
 templateFile, templateFileM,
 printerFile, printerFileM,
 layoutFile, layoutFileM,
 psFile, tFile, tFileM :: Options -> String
absFile       = mkFile withLangAbs "Abs" "hs"
absFileM      = mkMod  withLangAbs "Abs"
alexFile      = mkFile withLang "Lex" "x"
alexFileM     = mkMod  withLang "Lex"
happyFile     = mkFile withLang "Par" "y"
happyFileM    = mkMod  withLang "Par"
latexFile     = mkFile withLang "Doc" "tex"
txtFile       = mkFile withLang "Doc" "txt"
templateFile  = mkFile withLang "Skel" "hs"
templateFileM = mkMod  withLang "Skel"
printerFile   = mkFile withLang "Print" "hs"
printerFileM  = mkMod  withLang "Print"
dviFile       = mkFile withLang "Doc" "dvi"
psFile        = mkFile withLang "Doc" "ps"
gfAbs         = mkFile withLangAbs "" "Abs.gf"
gfConc        = mkFile withLang "" "Conc.gf"
tFile         = mkFile withLang "Test" "hs"
tFileM        = mkMod  withLang "Test"
errFile       = mkFile noLang   "ErrM" "hs"
errFileM      = mkMod  noLang   "ErrM"
shareFile     = mkFile noLang   "SharedString" "hs"
shareFileM    = mkMod  noLang   "SharedString"
layoutFileM   = mkMod  withLang "Layout"
layoutFile    = mkFile withLang "Layout" "hs"
cnfTablesFile = mkFile withLang "CnfTables" "hs"
cnfTablesFileM= mkMod  withLang "CnfTables"
xmlFileM      = mkMod  withLang "XML"
composOpFile  = mkFile noLang   "ComposOp" "hs"
composOpFileM = mkMod noLang    "ComposOp"


noLang :: Options -> String -> String
noLang _ name = name

withLang :: Options -> String -> String
withLang opts name = name ++ lang opts

withLangAbs :: Options -> String -> String
withLangAbs opts name = postp $ name ++ lang opts
  where
    postp nam = nam -- if multi opts then takeWhile (/='_') nam else nam

pkgToDir :: String -> FilePath
pkgToDir s = replace '.' pathSeparator s


mkMod :: (Options -> String -> String) -> String -> Options -> String
mkMod addLang name opts =
    pref ++ if inDir opts then lang opts ++ "." ++ name else addLang opts name
	where pref = maybe "" (++".") (inPackage opts)

mkFile :: (Options -> String -> String) -> String -> String -> Options -> FilePath
mkFile addLang name ext opts =
    pref ++ if inDir opts
       then lang opts ++ [pathSeparator] ++ name ++ ext'
       else addLang opts name ++ if null ext then "" else ext'
    where pref = maybe "" (\p->pkgToDir p++[pathSeparator]) (inPackage opts)
	  ext' = if null ext then "" else "." ++ ext
