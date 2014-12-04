module BNFC.Backend.Haskell.HsOpts where

import BNFC.Utils
import BNFC.Options
import System.FilePath (pathSeparator, (<.>))
import Data.List (intercalate)
import Data.Maybe (catMaybes)

type Options = SharedOptions

alex1 opts = alexMode opts == Alex1

absFile, absFileM,
 alexFile, alexFileM,
 composOpFile, composOpFileM,
 gfAbs,
 happyFile, happyFileM,
 errFile, errFileM,
 templateFile, templateFileM,
 printerFile, printerFileM,
 layoutFile, layoutFileM,
 tFile :: Options -> String
absFile       = mkFile withLang "Abs" "hs"
absFileM      = mkMod  withLang "Abs"
alexFile      = mkFile withLang "Lex" "x"
alexFileM     = mkMod  withLang "Lex"
happyFile     = mkFile withLang "Par" "y"
happyFileM    = mkMod  withLang "Par"
txtFile       = mkFile withLang "Doc" "txt"
templateFile  = mkFile withLang "Skel" "hs"
templateFileM = mkMod  withLang "Skel"
printerFile   = mkFile withLang "Print" "hs"
printerFileM  = mkMod  withLang "Print"
gfAbs         = mkFile withLang "" "Abs.gf"
tFile         = mkFile withLang "Test" "hs"
errFile       = mkFile noLang   "ErrM" "hs"
errFileM      = mkMod  noLang   "ErrM"
shareFile     = mkFile noLang   "SharedString" "hs"
shareFileM    = mkMod  noLang   "SharedString"
layoutFileM   = mkMod  withLang "Layout"
layoutFile    = mkFile withLang "Layout" "hs"
cnfTablesFile = mkFile withLang "CnfTables" "hs"
cnfTablesFileM= mkMod  withLang "CnfTables"
xmlFile       = mkFile withLang "XML" "hs"
xmlFileM      = mkMod  withLang "XML"
composOpFile  = mkFile noLang   "ComposOp" "hs"
composOpFileM = mkMod noLang    "ComposOp"


noLang :: Options -> String -> String
noLang _ name = name

withLang :: Options -> String -> String
withLang opts name = name ++ mkName [] CamelCase (lang opts)

pkgToDir :: String -> FilePath
pkgToDir s = replace '.' pathSeparator s


-- |
-- >>> mkMod withLang "Abstract" defaultOptions { lang = "abc" }
-- "AbstractAbc"
-- >>> mkMod noLang "Abstract" defaultOptions { lang = "abc" }
-- "Abstract"
-- >>> mkMod withLang "Abstract" defaultOptions { lang = "abc", inPackage = Just "A.B.C" }
-- "A.B.C.AbstractAbc"
-- >>> mkMod withLang "Abstract" defaultOptions { lang = "abc", inDir = True }
-- "Abc.Abstract"
-- >>> mkMod withLang "Abstract" defaultOptions { lang = "abc", inDir = True, inPackage = Just "A.B.C" }
-- "A.B.C.Abc.Abstract"
mkMod :: (Options -> String -> String) -> String -> Options -> String
mkMod addLang name opts = mkNamespace opts <.> mod
  where
    [] <.> s = s
    s1 <.> s2 = s1 ++ "." ++ s2
    mod | inDir opts = name
        | otherwise  = addLang opts name
-- |
-- >>> mkFile withLang "Abstract" "hs" defaultOptions { lang = "abc" }
-- "AbstractAbc.hs"
-- >>> mkFile noLang "Abstract" "hs" defaultOptions { lang = "abc" }
-- "Abstract.hs"
-- >>> mkFile withLang "Abstract" "" defaultOptions { lang = "abc" }
-- "AbstractAbc"
-- >>> mkFile noLang "Abstract" "" defaultOptions { lang = "abc" }
-- "Abstract"
-- >>> mkFile withLang "Abstract" "hs" defaultOptions { lang = "abc", inDir = True }
-- "Abc/Abstract.hs"
-- >>> mkFile withLang "Abstract" "hs" defaultOptions { lang = "abc", inDir = True, inPackage = Just "A.B.C" }
-- "A/B/C/Abc/Abstract.hs"
mkFile :: (Options -> String -> String) -> String -> String -> Options -> FilePath
mkFile addLang name ext opts = pkgToDir (mkMod addLang name opts) <.> ext

mkFileName :: String -> String -> FilePath
mkFileName module' ext = pkgToDir module' <.> ext


-- | Determine the modules' namespace
--
-- >>> mkNamespace defaultOptions
-- ""
-- >>> mkNamespace defaultOptions { lang = "Bla", inDir = True }
-- "Bla"
-- >>> mkNamespace defaultOptions { inPackage = Just "My.Cool.Package" }
-- "My.Cool.Package"
-- >>> mkNamespace defaultOptions { lang = "bla_bla", inDir = True }
-- "BlaBla"
-- >>> mkNamespace defaultOptions { lang = "bla", inDir = True, inPackage = Just "P"}
-- "P.Bla"
mkNamespace :: Options -> FilePath
mkNamespace opts = intercalate "." $ catMaybes [inPackage opts, dir]
  where
    dir | inDir opts = Just (mkName [] CamelCase (lang opts))
        | otherwise  = Nothing

-- | Determine the directory corresponding to the modules' namespace
--
-- >>> codeDir defaultOptions
-- ""
-- >>> codeDir defaultOptions { lang = "Bla", inDir = True }
-- "Bla"
-- >>> codeDir defaultOptions { inPackage = Just "My.Cool.Package" }
-- "My/Cool/Package"
-- >>> codeDir defaultOptions { lang = "bla_bla", inDir = True }
-- "BlaBla"
-- >>> codeDir defaultOptions { lang = "bla", inDir = True, inPackage = Just "P"}
-- "P/Bla"
codeDir :: Options -> FilePath
codeDir = pkgToDir . mkNamespace
