{-
    BNF Converter: FSharp main file
    Copyright (C) 2021  Author:  Grzegorz Dziadkiewicz

-}

-- based on BNFC OCaml backend

{-# LANGUAGE QuasiQuotes #-}

module BNFC.Backend.FSharp (makeFSharp) where

import System.FilePath (pathSeparator, (</>))

import BNFC.Backend.Base                    (MkFiles, mkfile)
import BNFC.Backend.Common.Makefile
import BNFC.Backend.FSharp.CFtoFSharpAbs
import BNFC.Backend.FSharp.CFtoFsLex
import BNFC.Backend.FSharp.CFtoFSharpPrinter
import BNFC.Backend.FSharp.CFtoFSharpShow
import BNFC.Backend.FSharp.CFtoFSharpTemplate
import BNFC.Backend.FSharp.CFtoFSharpTest     (fsharpTestfile)
import BNFC.Backend.FSharp.CFtoFsYacc
import qualified BNFC.Backend.XML as XML
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options
import BNFC.Utils
import qualified BNFC.Backend.Common.Makefile as Makefile

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

-- | Turn language name into a valid fsharp module identifier.
sanitizedLang :: SharedOptions -> String
sanitizedLang = camelCase_ . lang


absFile, absFileM, fslexFile, fslexFileM, fsyaccFile, fsyaccFileM,
  utilFile, utilFileM, templateFile, templateFileM, printerFile, printerFileM,
  tFile, tFileM, showFile, showFileM, fsprojFile, buildTarget :: SharedOptions -> String
absFile       = mkFile withLang "Abs" "fs"
absFileM      = mkMod  withLang "Abs"
fslexFile     = mkFile withLang "Lex" "fsl"
fslexFileM    = mkMod  withLang "Lex"
fsyaccFile    = mkFile withLang "Par" "fsy"
fsyaccFileM   = mkMod  withLang "Par"
templateFile  = mkFile withLang "Skel" "fs"
templateFileM = mkMod  withLang "Skel"
printerFile   = mkFile withLang "Print" "fs"
printerFileM  = mkMod  withLang "Print"
showFile      = mkFile withLang "Show" "fs"
showFileM     = mkMod  withLang "Show"
tFileM        = mkMod  withLang "Test"
tFile         = mkFile withLang "Test" "fs"
utilFileM     = mkMod  noLang   "BnfcUtil"
utilFile      = mkFile noLang   "BnfcUtil" "fs"
fsprojFile    = mkFile withLang  "" "fsproj"
buildTarget   = mkFile withLang  "" ""

makeFSharp :: SharedOptions -> CF -> MkFiles ()
makeFSharp opts cf = do
  let absMod = absFileM opts
      lexMod = fslexFileM opts
      parMod = fsyaccFileM opts
      prMod  = printerFileM opts
      showMod = showFileM opts
      tFileMod = tFileM opts
  do
    mkfile (absFile opts)       comment $ cf2Abstract absMod cf
    mkfile (fslexFile opts)  comment $ cf2fslex lexMod parMod cf
    mkfile (fsyaccFile opts) C.comment $
                 cf2fsyacc parMod absMod lexMod  cf
    mkfile (templateFile opts)  comment $ cf2Template (templateFileM opts) absMod cf
    mkfile (printerFile opts)   comment $ cf2Printer prMod absMod cf
    mkfile (showFile opts)      comment $ cf2show showMod absMod cf
    mkfile (tFile opts)         comment $ fsharpTestfile absMod lexMod parMod prMod showMod tFileMod cf
    mkfile (utilFile opts)      comment $ utilM (utilFileM opts)
    mkfile (fsprojFile opts)    XML.comment $ fsprojM opts
    mkMakefile opts $ makefile opts
    -- case xml opts of
    --   2 -> makeXML opts True cf
    --   1 -> makeXML opts False cf
    --   _ -> return ()

-- | Generate the makefile.
makefile
  :: SharedOptions
  -> String    -- ^ Filename of the makefile.
  -> Doc       -- ^ Content of the makefile.
makefile opts makeFile = vcat
  [ "# Makefile for building the parser and test program."
  , phonyRule
  , defaultRule
  , vcat [ "# Rules for building the parser." , "" ]
  -- If option -o was given, we have no access to the grammar file
  -- from the Makefile.  Thus, we have to drop the rule for
  -- reinvokation of bnfc.
  , when (isDefault outDir opts) $ bnfcRule
  , testParserRule
  , vcat [ "# Rules for cleaning generated files." , "" ]
  , cleanRule
  , distCleanRule
  , "# EOF"
  ]
  where
  -- | List non-file targets here.
  phonyRule :: Doc
  phonyRule = vcat
    [ "# List of goals not corresponding to file names."
    , ""
    , Makefile.mkRule ".PHONY" [ "all", "clean", "distclean" ] []
    ]
  -- | Default: build test parser(s).
  defaultRule :: Doc
  defaultRule = vcat
     [ "# Default goal."
     , ""
     , Makefile.mkRule "all" tgts []
     ]
     where
     tgts = [ buildTarget opts ]

  -- | Rule to build F# test parser.
  testParserRule :: Doc
  testParserRule = Makefile.mkRule tgt deps [ "dotnet build" ]
    where
    tgt :: String
    tgt = buildTarget opts
    deps :: [String]
    deps = map ($ opts)
      [   absFile
        , printerFile
        , tFile
        , fslexFile
        , fsyaccFile
        , templateFile
        , showFile
        , utilFile
        , fsprojFile
      ]
  cleanRule =
      mkRule "clean" []
          [ "-rm -fr bin obj "]

  distCleanRule =
       mkRule "distclean" ["clean"]
          [ "-rm -f " ++ unwords [ mkFile withLang "Lex" "*" opts,
                                   mkFile withLang "Par" "*" opts,
                                   mkFile withLang "Layout" "*" opts,
                                   mkFile withLang "Skel" "*" opts,
                                   mkFile withLang "Print" "*" opts,
                                   mkFile withLang "Show" "*" opts,
                                   mkFile withLang "Test" "*" opts,
                                   mkFile withLang "Abs" "*" opts,
                                   mkFile withLang "Test" "" opts,
                                   mkFile withLang  "" "fsproj" opts,
                                   utilFile opts,
                                   makeFile ]]

  -- | Rule to reinvoke @bnfc@ to updated parser.
  --   Reinvokation should not recreate @Makefile@!
  bnfcRule :: Doc
  bnfcRule = Makefile.mkRule tgts [ lbnfFile opts ] [ recipe ]
      where
      recipe    = unwords [ "bnfc", printOptions opts{ make = Nothing } ]
      tgts      = unwords . map ($ opts) $
        [ absFile
        , fslexFile
        , fsyaccFile
        , utilFile
        , templateFile
        , printerFile
        , tFile
        , showFile
        ]

comment :: String -> String
comment x = unwords [ "(*", x, "*)" ]

pkgToDir :: String -> FilePath
pkgToDir = replace '.' pathSeparator

utilM :: String -> String
utilM moduleName = unlines
    ["//automatically generated by BNFC",
     "module" +++ moduleName,
     "open FSharp.Text.Lexing",
     "",
     "exception ParseError of Position * Position "
    ]

fsprojM :: SharedOptions -> String
fsprojM opts = unlines
  ["<Project Sdk=\"Microsoft.NET.Sdk\">"
  ,""
  ,"  <PropertyGroup>"
  ,"    <OutputType>Exe</OutputType>"
  ,"    <TargetFramework>net5.0</TargetFramework>"
  ,"    <WarnOn>3390;$(WarnOn)</WarnOn>"
  ,"  </PropertyGroup>"
  ,""
  ,"  <ItemGroup>"
  ,"    <Compile Include=\"" ++ utilFile opts ++ "\" />"
  ,"    <Compile Include=\"" ++ absFile opts ++ "\" />"
  ,"    <FsYacc  Include=\"" ++ fsyaccFile opts ++ "\" >"
  ,"      <OtherFlags>--module " ++ fsyaccFileM opts ++ "</OtherFlags>"
  ,"    </FsYacc>"
  ,"    <FsLex   Include=\"" ++ fslexFile opts ++ "\">"
  ,"      <OtherFlags>--unicode</OtherFlags>"
  ,"    </FsLex>"
  ,"    <Compile Include=\"" ++ fsyaccFileM opts ++ ".fsi\" />"
  ,"    <Compile Include=\"" ++ fsyaccFileM opts ++ ".fs\" />"
  ,"    <Compile Include=\"" ++ fslexFileM opts ++ ".fs\" />"
  ,"    <Compile Include=\"" ++ printerFile opts ++ "\" />"
  ,"    <Compile Include=\"" ++ showFile opts ++ "\" />"
  ,"    <Compile Include=\"" ++ templateFile opts ++ "\" />"
  ,"    <Compile Include=\"" ++ tFile opts ++ "\" />"
  ,"  </ItemGroup>"
  ,""
  ,"  <ItemGroup>"
  ,"    <PackageReference Include=\"FsLexYacc\" Version=\"10.2.0\" />"
  ,"  </ItemGroup>"

  ,"</Project>"
  ]
