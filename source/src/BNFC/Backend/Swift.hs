{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Swift ( makeSwift ) where

import Text.PrettyPrint ( text, vcat, render, nest )

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator)
import System.Directory ( createDirectoryIfMissing )
import Data.Char (toLower)

import BNFC.Backend.Base (MkFiles, mkfile,liftIO)
import BNFC.CF (CF, getAbstractSyntax, firstEntry, catToStr, identCat, normCat )
import BNFC.Options (SharedOptions (Options, inPackage, lang, optMake, dLanguage, antlrOpts, outDir), AntlrTarget (Swift))
import BNFC.Utils (mkName, NameStyle (SnakeCase), replace, (+.+), (+++))
import BNFC.Backend.Common.Makefile as MakeFile 
import BNFC.Backend.Common.NamedVariables (firstUpperCase, firstLowerCase) 
import BNFC.Backend.Antlr (makeAntlr, makeAntlr', DirectoryOptions (DirectoryOptions, baseDirectory, nameStyle))
import BNFC.Backend.Swift.CFtoSwiftAST ( cf2SwiftAST )
import BNFC.Backend.Swift.CFtoSwiftBuilder ( cf2SwiftBuilder )
import BNFC.Backend.Swift.Common ( indent, buildVariableTypeFromSwiftType, cat2SwiftType, cat2SwiftClassName )

makeSwift :: SharedOptions -> CF -> MkFiles ()
makeSwift opts@Options{..} cf = do
    let dirBase = replace '.' pathSeparator $ packageName
        -- langBase = dirBase </> (langName ++ "_generated")
        -- libLang = langBase </> "lib"
        -- srcLang = libLang </> "src"
        -- libBase = dirBase </> "lib"
        -- binBase = dirBase </> "bin"

        -- directoryOptions = DirectoryOptions{baseDirectory = Just dirBase, nameStyle = Just SnakeCase}
 
    -- Generates files in an incorrect place

    makeAntlr (opts {dLanguage = Swift, optMake = Nothing}) cf
    -- makeAntlr (opts {dLanguage = Swift, optMake = Nothing}) cf
    MakeFile.mkMakefile optMake $ makefileContent dirBase

    mkfile (dirBase </> "ast.swift") makeSwiftComment astContent
    mkfile (dirBase </> "builder.swift") makeSwiftComment builderContent

  where
    astContent = cf2SwiftAST (firstUpperCase langName) cf
    -- builderContent = cf2SwiftBuilder (firstUpperCase langName) cf
    builderContent = cf2SwiftBuilder cf opts
    mainContent = unlines 
      [ "import '../lib/test.swift';"
      , "void main(List<String> args) {"
      , "  final test = Test();"
      , "  test.run(args);"
      , "}" ]
    packageName = maybe id (+.+) inPackage $ mkName [] SnakeCase lang
    langName = firstLowerCase $ mkName [] SnakeCase lang
    langNameUpperCased = firstUpperCase langName
    importLangName = "import 'package:" ++ langName ++ "_generated/" ++ langName ++ "_generated.swift';"

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
      ] ++ deps ++ [ "dev_dependencies:"
      , "  lints: ^4.0.0" ])

    lexerClassName = lang ++ "GrammarLexer"
    parserClassName = lang ++ "GrammarParser"

    makeVars x = [MakeFile.mkVar n v | (n,v) <- x]
    makeRules x = [MakeFile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]

    makefileVars = vcat $ makeVars
      [("LANG", langNameUpperCased)
      , ("LEXER_NAME", langNameUpperCased ++ "Lexer")
      , ("PARSER_NAME", langNameUpperCased ++ "Parser")
      -- , ("ANTLR4", "java -Xmx500M -cp \"/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH\" org.antlr.v4.Tool")
      , ("ANTLR4", "antlr4") -- installed using pip
      ]

    refVarInSrc dirBase refVar = dirBase </> MakeFile.refVar refVar

    rmFile :: (String -> String) -> String -> String -> String
    rmFile refSrcVar refVar ext = "rm -f" +++ refSrcVar refVar ++ ext

    makefileRules refSrcVar = 
      let rmInSrc = rmFile refSrcVar
      in vcat $ makeRules
        [ (".PHONY", ["all", "clean", "remove"], [])
        , ("all", [MakeFile.refVar "LANG"], [])
        , ("lexer"
            , [refSrcVar "LEXER_NAME" ++ ".g4"]
            , [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=Swift" +++ refSrcVar "LEXER_NAME" ++ ".g4"])
        , ("parser"
            , [refSrcVar "PARSER_NAME" ++ ".g4"]
            , [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=Swift" +++ "-no-listener" +++ "-no-visitor" +++ refSrcVar "PARSER_NAME" ++ ".g4"])
        , ("install-deps-external"
            , [MakeFile.refVar "LANG" </> "pubspec.yaml"]
            , ["cd" +++ (MakeFile.refVar "LANG") ++ "; Swift pub get"])
        , ("install-deps-internal"
            , [MakeFile.refVar "LANG" </> (MakeFile.refVar "LANG" ++ "_generated") </> "pubspec.yaml"]
            , ["cd" +++ (MakeFile.refVar "LANG" </> (MakeFile.refVar "LANG" ++ "_generated")) ++ "; Swift pub get"])
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

    makefileContent dirBase _ = vcat [makefileVars, "", makefileRules $ refVarInSrc dirBase, ""]

makeSwiftComment :: String -> String
makeSwiftComment = ("// Swift " ++)

makeSwiftCommentYaml :: String -> String
makeSwiftCommentYaml = ("# Swift" ++)

toLowerCase :: String -> String
toLowerCase = map toLower
