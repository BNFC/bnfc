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
import BNFC.Utils (mkName, NameStyle (SnakeCase, CamelCase), replace, (+.+), (+++))
import BNFC.Backend.Common.Makefile as MakeFile 
import BNFC.Backend.Common.NamedVariables (firstUpperCase, firstLowerCase) 
import BNFC.Backend.Antlr (makeAntlr, makeAntlr', DirectoryOptions (DirectoryOptions, baseDirectory, nameStyle))
import BNFC.Backend.Swift.CFtoSwiftAST ( cf2SwiftAST )
import BNFC.Backend.Swift.CFtoSwiftBuilder ( cf2SwiftBuilder )
import BNFC.Backend.Swift.CFtoSwiftSkeleton ( cf2SwiftSkeleton )
import BNFC.Backend.Swift.CFtoSwiftPrinter ( cf2SwiftPrinter )
import BNFC.Backend.Swift.Common ( indent, buildVariableTypeFromSwiftType, cat2SwiftType, cat2SwiftClassName, mkBuildFnName )

makeSwift :: SharedOptions -> CF -> MkFiles ()
makeSwift opts@Options{..} cf = do
    let dirBase = replace '.' pathSeparator $ packageName
        sourcesDir = dirBase </> "Sources"
        targetDir = sourcesDir </> langNameUpperCased
        directoryOptions = DirectoryOptions{baseDirectory = Just targetDir, nameStyle = Just CamelCase}

    makeAntlr' (opts {dLanguage = Swift, optMake = Nothing}) cf directoryOptions

    MakeFile.mkMakefile optMake $ makefileContent targetDir

    mkfile (targetDir </> "AbstractSyntaxTree.swift") makeSwiftComment astContent
    mkfile (targetDir </> "Builder.swift") makeSwiftComment builderContent
    mkfile (targetDir </> "Printer.swift") makeSwiftComment printerContent
    mkfile (targetDir </> langNameUpperCased ++ ".swift") makeSwiftComment (publicApiContent langNameUpperCased)
    mkfile (dirBase </> "Package.swift") makePackageHeader (packageFileContent langNameUpperCased)
    mkfile "Skeleton.swift" makeSwiftComment skeletonContent
  where
    packageName = maybe id (+.+) inPackage $ mkName [] CamelCase lang
    langName = firstLowerCase $ mkName [] CamelCase lang
    langNameUpperCased = firstUpperCase langName

    astContent = cf2SwiftAST langNameUpperCased cf
    builderContent = cf2SwiftBuilder cf opts
    skeletonContent = cf2SwiftSkeleton langNameUpperCased cf
    printerContent = cf2SwiftPrinter cf

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
        , (MakeFile.refVar "LANG", ["lexer", "parser", "clean"], [])
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
    
    -- Content of Package.swift, uses to declare swift package
    packageFileContent langName = vcat
      [ "import PackageDescription"
      , ""
      , "let package = Package("
      , nest 2 $ vcat
        [ text $ "name: \"" ++ langName ++ "\","
        , "products: ["
        , nest 2 $ vcat 
          [ ".library("
          , text $ "  name: \"" ++ langName ++ "\","
          , text $ "  targets: [\"" ++ langName ++ "\"]"
          , ")"
          ]
        , "],"
        ]
      , nest 2 $ vcat
        [ "dependencies: ["
        , "  .package(name: \"Antlr4\", url: \"https://github.com/antlr/antlr4\", from: \"4.12.0\")"
        , "],"
        ]
      , nest 2 $ vcat
        [ "targets: ["
        , text $ "  .target(name: \"" ++ langName ++ "\", dependencies: [\"Antlr4\"])"
        , "]"
        ]
      , ")"
      ]
    
    publicApiContent langName = vcat
      [ "import Antlr4"
      , ""
      , text $ "public func ast(from text: String) -> Result<" ++ catToStr firstCat ++ ", Error> {"
      , nest 2 $ vcat
        [ "let input = ANTLRInputStream(text)"
        , text $ "let lexer =" +++ langName ++ "Lexer(input)"
        , "let tokens = CommonTokenStream(lexer)"
        , "do {"
        , nest 2 $ vcat
          [ text $ "let parser = try" +++ langName ++ "Parser(tokens)"
          , text $ "let ctx = try parser." ++ (firstLowerCase $ identCat $ normCat firstCat) ++ "()"
          , text $ "let program = try" +++ mkBuildFnName firstCat ++ "(ctx)"
          , "return .success(program)"
          ]
        , "} catch {"
        , "  return .failure(error)"
        , "}"
        ]
      , "}"
      ]
      where 
        firstCat = firstEntry cf


makeSwiftComment :: String -> String
makeSwiftComment = ("// Swift " ++)

makePackageHeader :: String -> String
makePackageHeader str = toolingVersion ++ "\n" ++ (makeSwiftComment str)
  where
    toolingVersion = "// swift-tools-version: 5.9"