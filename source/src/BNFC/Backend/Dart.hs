{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Dart ( makeDart ) where

import Text.PrettyPrint ( text, vcat, render, nest )

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator)
import System.Directory ( createDirectoryIfMissing )
import Data.Char (toLower)

import BNFC.Backend.Base (MkFiles, mkfile,liftIO)
import BNFC.CF (CF, getAbstractSyntax)
import BNFC.Options (SharedOptions (Options, inPackage, lang, optMake, dLanguage, antlrOpts, outDir), AntlrTarget (Dart))
import BNFC.Utils (mkName, NameStyle (SnakeCase), replace, (+.+), (+++))
import BNFC.Backend.Common.Makefile as MakeFile
import BNFC.Backend.Antlr (makeAntlr)
import BNFC.Backend.Dart.CFtoDartAST ( cf2DartAST )
import BNFC.Backend.Dart.CFtoDartBuilder ( cf2DartBuilder )
import BNFC.Backend.Dart.CFtoDartPrinter ( cf2DartPrinter )
import BNFC.Backend.Dart.CFtoDartSkeleton ( cf2DartSkeleton )

makeDart :: SharedOptions -> CF -> MkFiles ()
makeDart opts@Options{..} cf = do
    let packageBase = maybe id (+.+) inPackage pkgName
        dirBase = pkgToDir packageBase
        langBase = dirBase </> lang
        libLang = langBase </> "lib"
        srcLang = libLang </> "src"
        libBase = dirBase </> "lib"
        binBase = dirBase </> "bin"
 
    -- Generates files in an incorrect place
    makeAntlr (opts {dLanguage = Dart, optMake = Nothing}) cf
    MakeFile.mkMakefile optMake makefileContent

    mkfile (srcLang </> "ast.dart") makeDartComment astContent
    mkfile (srcLang </> "builder.dart") makeDartComment builderContent
    mkfile (srcLang </> "printer.dart") makeDartComment printerContent
    mkfile (libLang </> "stella.dart") makeDartComment stellaExportsContent
    mkfile (langBase </> "pubspec.yaml") makeDartCommentYaml 
      $ pubspecContent 
          lang 
          ("A module with the AST, Pretty-Printer and AST-builder for" +++ lang) 
          []
    mkfile (libBase </> "runner.dart") makeDartComment runnerContent
    mkfile (libBase </> "skeleton.dart") makeDartComment skeletonContent
    mkfile (binBase </> "main.dart") makeDartComment mainContent
    mkfile (dirBase </> "pubspec.yaml" ) makeDartCommentYaml 
      $ pubspecContent 
          (lang ++ "_example") 
          ("A simple project for" +++ lang) 
          [ lang ++ ":", "  path:" +++ lang ]

  where
    astContent = cf2DartAST cf
    builderContent = cf2DartBuilder cf lang
    printerContent = cf2DartPrinter cf
    skeletonContent = cf2DartSkeleton cf
    stellaExportsContent = unlines
      [ "export 'src/ast.dart';"
      , "export 'src/builder.dart';" 
      , "export 'src/printer.dart';" ]
    runnerContent = unlines 
      [ "import 'package:stella/stella.dart';"
      , "class Runner {"
      , "}" ]
    mainContent = unlines 
      [ "import '../lib/runner.dart'"
      , "void main(List<String> args) {"
      , "  final runner = Runner();"
      , "  runner.run();"
      , "}" ]
    pkgName = mkName [] SnakeCase lang
    pkgToDir = replace '.' pathSeparator

    pubspecContent moduleName desc deps = unlines 
      ([ "name:" +++ moduleName 
      , "description:" +++ desc
      , "version: 1.0.0"
      , "publish_to: 'none'"
      , "environment:"
      , "  sdk: ^3.3.4"
      , "dependencies:"
      , "  antlr4: ^4.13.1"
      , "  fast_immutable_collections: ^10.2.2" 
      ] ++ (map ("  " ++) deps) ++ [ "dev_dependencies:"
      , "  lints: ^3.0.0" ])

    lexerClassName = lang ++ "GrammarLexer"
    parserClassName = lang ++ "GrammarParser"

    makeVars x = [MakeFile.mkVar n v | (n,v) <- x]
    makeRules x = [MakeFile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]

    makefileVars = vcat $ makeVars
      [("LANG", lang)
      , ("LEXER_NAME", lang ++ "Lexer")
      , ("PARSER_NAME", lang ++ "Parser")
      , ("ANTLR4", "java org.antlr.v4.Tool")
      ]

    refVarWithPrefix :: String -> String
    refVarWithPrefix refVar = MakeFile.refVar "LANG" </> MakeFile.refVar refVar

    rmFile :: String -> String -> String
    rmFile refVar ext = "rm -f" +++ refVarWithPrefix refVar ++ ext

    makefileRules =  vcat $ makeRules
      [ (".PHONY", ["all", "clean", "remove"], [])
      , ("all", [MakeFile.refVar "LANG"], [])
      , ("lexer", [refVarWithPrefix "LEXER_NAME" ++ ".g4"], [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=Dart" +++ refVarWithPrefix "LEXER_NAME" ++ ".g4"])
      , ("parser", [refVarWithPrefix "PARSER_NAME" ++ ".g4"], [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=Dart" +++ refVarWithPrefix "PARSER_NAME" ++ ".g4"])
      , ("install-deps", [MakeFile.refVar "LANG" </> "pubspec.yaml"], ["cd" +++ (MakeFile.refVar "LANG") ++ "; dart pub get"])
      , (MakeFile.refVar "LANG", ["lexer", "parser", "install-deps"], [])
      , ("clean", [],
        [ 
          rmFile "LEXER_NAME" ".interp"
        , rmFile "LEXER_NAME" ".tokens"
        , rmFile "PARSER_NAME" ".interp"
        , rmFile "PARSER_NAME" ".tokens"
        , rmFile "LEXER_NAME" ".dart"
        , rmFile "PARSER_NAME" ".dart"
        , rmFile "PARSER_NAME" "Listener.dart"
        ])
      , ("remove", [], ["rm -rf" +++ MakeFile.refVar "LANG"])
      ]

    makefileContent _ = vcat [makefileVars, "", makefileRules, ""]

makeDartComment :: String -> String
makeDartComment = ("// Dart " ++)

makeDartCommentYaml :: String -> String
makeDartCommentYaml = ("# Dart" ++)

toLowerCase :: String -> String
toLowerCase = map toLower
