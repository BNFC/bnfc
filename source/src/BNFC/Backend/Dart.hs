{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Dart ( makeDart ) where

import Text.PrettyPrint ( text, vcat, render, nest )

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator)
import System.Directory ( createDirectoryIfMissing )
import Data.Char (toLower)

import BNFC.Backend.Base (MkFiles, mkfile,liftIO)
import BNFC.CF (CF, getAbstractSyntax, firstEntry, catToStr, identCat, normCat )
import BNFC.Options (SharedOptions (Options, inPackage, lang, optMake, dLanguage, antlrOpts, outDir), AntlrTarget (Dart))
import BNFC.Utils (mkName, NameStyle (SnakeCase), replace, (+.+), (+++))
import BNFC.Backend.Common.Makefile as MakeFile 
import BNFC.Backend.Common.NamedVariables (firstUpperCase, firstLowerCase) 
import BNFC.Backend.Antlr (makeAntlr', DirectoryOptions (DirectoryOptions, baseDirectory, nameStyle))
import BNFC.Backend.Dart.CFtoDartAST ( cf2DartAST )
import BNFC.Backend.Dart.CFtoDartBuilder ( cf2DartBuilder )
import BNFC.Backend.Dart.CFtoDartPrinter ( cf2DartPrinter )
import BNFC.Backend.Dart.CFtoDartSkeleton ( cf2DartSkeleton )
import BNFC.Backend.Dart.Common ( indent, buildVariableTypeFromDartType, cat2DartType, cat2DartClassName )

makeDart :: SharedOptions -> CF -> MkFiles ()
makeDart opts@Options{..} cf = do
    let dirBase = replace '.' pathSeparator $ packageName
        langBase = dirBase </> (langName ++ "_generated")
        libLang = langBase </> "lib"
        srcLang = libLang </> "src"
        libBase = dirBase </> "lib"
        binBase = dirBase </> "bin"
        directoryOptions = DirectoryOptions{baseDirectory = Just srcLang, nameStyle = Just SnakeCase}
 
    -- Generates files in an incorrect place

    makeAntlr' (opts {dLanguage = Dart, optMake = Nothing}) cf directoryOptions
    MakeFile.mkMakefile optMake $ makefileContent srcLang

    mkfile (srcLang </> "ast.dart") makeDartComment astContent
    mkfile (srcLang </> "builder.dart") makeDartComment builderContent
    mkfile (srcLang </> "pretty_printer.dart") makeDartComment printerContent
    mkfile (libLang </> (langName ++ "_generated.dart")) makeDartComment exportsContent
    mkfile (langBase </> "pubspec.yaml") makeDartCommentYaml 
      $ pubspecContent 
          (langName ++ "_generated")
          ("A module with the AST, Pretty-Printer and AST-builder for" +++ langName) 
          []
    mkfile (libBase </> "test.dart") makeDartComment testContent
    mkfile (libBase </> "skeleton.dart") makeDartComment skeletonContent
    mkfile (binBase </> "main.dart") makeDartComment mainContent
    mkfile (dirBase </> "pubspec.yaml" ) makeDartCommentYaml 
      $ pubspecContent 
          (langName ++ "_example") 
          ("A simple project for" +++ langName) 
          [ langName ++ "_generated:", "  path:" +++ langName ++ "_generated" ]

  where
    astContent = cf2DartAST (firstUpperCase langName) cf
    builderContent = cf2DartBuilder (firstUpperCase langName) cf
    printerContent = cf2DartPrinter (firstUpperCase langName) cf
    skeletonContent = cf2DartSkeleton (firstUpperCase langName) cf importLangName
    exportsContent = unlines
      [ "export 'src/ast.dart';"
      , "export 'src/builder.dart';" 
      , "export 'src/pretty_printer.dart';"
      , "export 'src/" ++ langName ++ "_lexer.dart';"
      , "export 'src/" ++ langName ++ "_parser.dart';" ]
    testContent = 
      let 
        firstCat = firstEntry cf 
        varType = buildVariableTypeFromDartType $ cat2DartType (firstUpperCase langName) firstCat
        varName = cat2DartClassName langName firstCat
        rawVarName = firstLowerCase $ identCat $ normCat firstCat
      in unlines (
        [ "import 'package:antlr4/antlr4.dart';"
        , "import 'package:fast_immutable_collections/fast_immutable_collections.dart';"
        , importLangName
        , "import 'skeleton.dart';"
        , "class Test {" 
        , "  Future<void> run(List<String> arguments) async {" ] 
        ++ ( indent 2 
            [ "final input = await InputStream.fromString(arguments[0]);"
            , "final lexer =" +++ langName ++ "_lexer(input);"
            , "final tokens = CommonTokenStream(lexer);"
            , "final parser =" +++ langName ++ "_parser(tokens);"
            , "parser.addErrorListener(DiagnosticErrorListener());"
            , "final output = build" ++ varName ++ "(parser." ++ rawVarName ++ "());"
            , "print('\"Parse Successful!\"\\n');"
            , "print('\"[Abstract Syntax]\"\\n');"
            , "print('${output?.print}\\n');"
            , "print('\"[Linearized Tree]\"\\n');"
            , "print(switch (output) {"
            , "  null => '" ++ varType ++ " is null',"
            , "  " ++ varType ++ " p => interpret" ++ varName ++ "(p),"
            , "});" 
            ] ) 
        ++ [ "  }", "}" ] )
    mainContent = unlines 
      [ "import '../lib/test.dart';"
      , "void main(List<String> args) {"
      , "  final test = Test();"
      , "  test.run(args);"
      , "}" ]
    packageName = maybe id (+.+) inPackage $ mkName [] SnakeCase lang
    langName = firstLowerCase $ mkName [] SnakeCase lang
    importLangName = "import 'package:" ++ langName ++ "_generated/" ++ langName ++ "_generated.dart';"

    pubspecContent moduleName desc deps = unlines (
      [ "name:" +++ moduleName 
      , "description:" +++ desc
      , "version: 1.0.0"
      , "publish_to: 'none'"
      , "environment:"
      , "  sdk: ^3.4.0"
      , "dependencies:"
      , "  antlr4: ^4.13.1"
      , "  fast_immutable_collections: ^10.2.2" 
      ] ++ (indent 1 deps) ++ [ "dev_dependencies:"
      , "  lints: ^4.0.0" ])

    lexerClassName = lang ++ "GrammarLexer"
    parserClassName = lang ++ "GrammarParser"

    makeVars x = [MakeFile.mkVar n v | (n,v) <- x]
    makeRules x = [MakeFile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]

    makefileVars = vcat $ makeVars
      [("LANG", langName)
      , ("LEXER_NAME", langName ++ "_lexer")
      , ("PARSER_NAME", langName ++ "_parser")
      , ("ANTLR4", "java -Xmx500M -cp \"/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH\" org.antlr.v4.Tool")
      ]

    refVarInSrc srcLang refVar = srcLang </> MakeFile.refVar refVar

    rmFile :: (String -> String) -> String -> String -> String
    rmFile refSrcVar refVar ext = "rm -f" +++ refSrcVar refVar ++ ext

    makefileRules refSrcVar = 
      let rmInSrc = rmFile refSrcVar
      in vcat $ makeRules
        [ (".PHONY", ["all", "clean", "remove"], [])
        , ("all", [MakeFile.refVar "LANG"], [])
        , ("lexer"
            , [refSrcVar "LEXER_NAME" ++ ".g4"]
            , [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=Dart" +++ refSrcVar "LEXER_NAME" ++ ".g4"])
        , ("parser"
            , [refSrcVar "PARSER_NAME" ++ ".g4"]
            , [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=Dart" +++ "-no-listener" +++ "-no-visitor" +++ refSrcVar "PARSER_NAME" ++ ".g4"])
        , ("install-deps-external"
            , [MakeFile.refVar "LANG" </> "pubspec.yaml"]
            , ["cd" +++ (MakeFile.refVar "LANG") ++ "; dart pub get"])
        , ("install-deps-internal"
            , [MakeFile.refVar "LANG" </> (MakeFile.refVar "LANG" ++ "_generated") </> "pubspec.yaml"]
            , ["cd" +++ (MakeFile.refVar "LANG" </> (MakeFile.refVar "LANG" ++ "_generated")) ++ "; dart pub get"])
        , (MakeFile.refVar "LANG", ["lexer", "parser", "clean", "install-deps-external", "install-deps-internal"], [])
        , ("clean", [],
          [ 
            rmInSrc "LEXER_NAME" ".interp"
          , rmInSrc "LEXER_NAME" ".tokens"
          , rmInSrc "PARSER_NAME" ".interp"
          , rmInSrc "PARSER_NAME" ".tokens"
          , rmInSrc "LEXER_NAME" ".g4"
          , rmInSrc "PARSER_NAME" ".g4"
          ])
        , ("remove", [], ["rm -rf" +++ MakeFile.refVar "LANG"])
        ]

    makefileContent srcLang _ = vcat [makefileVars, "", makefileRules $ refVarInSrc srcLang, ""]

makeDartComment :: String -> String
makeDartComment = ("// Dart " ++)

makeDartCommentYaml :: String -> String
makeDartCommentYaml = ("# Dart" ++)

toLowerCase :: String -> String
toLowerCase = map toLower
