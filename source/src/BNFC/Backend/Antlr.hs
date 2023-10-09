{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Antlr ( makeAntlr ) where

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator)
import Text.PrettyPrint.HughesPJ (vcat)

import BNFC.Utils
import BNFC.CF
import BNFC.Options as Options
import BNFC.Backend.Base
import BNFC.Backend.Antlr.CFtoAntlr4Lexer
import BNFC.Backend.Antlr.CFtoAntlr4Parser
import BNFC.Backend.Antlr.Utils (getAntlrFlags, dotG4)
import BNFC.Backend.Common.Makefile as MakeFile

makeAntlr :: SharedOptions -> CF -> MkFiles ()
makeAntlr opts@Options{..} cf = do
  let packageBase = maybe id (+.+) inPackage pkg
      dirBase = pkgToDir packageBase

  let (lex, env) = cf2AntlrLex packageBase cf
    -- Where the lexer file is created. lex is the content!
  mkfile (dirBase </> mkG4Filename "Lexer") mkAntlrComment lex

  let parserContent = cf2AntlrParse packageBase cf linenumbers env
  mkfile (dirBase </> mkG4Filename "Parser") mkAntlrComment parserContent

  MakeFile.mkMakefile optMake makefileContent
    where
      pkg = mkName [] CamelCase lang
      pkgToDir = replace '.' pathSeparator
      mkG4Filename = dotG4 . (lang ++)

      makeVars x = [MakeFile.mkVar n v | (n,v) <- x]
      makeRules x = [MakeFile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]

      otherFlags = getAntlrFlags opts
      langRef = MakeFile.refVar "LANG"

      makefileVars = vcat $ makeVars
        [ ("LANG", lang)
        , ("LEXER_NAME", langRef ++ "Lexer")
        , ("PARSER_NAME", langRef ++ "Parser")
        , ("ANTLR4", "java org.antlr.v4.Tool")
        , ("DLANGUAGE", parseAntlrTarget dLanguage)
        , ("OTHER_FLAGS", otherFlags)
        ]

      refVarWithPrefix = (langRef </>) . MakeFile.refVar

      genAntlrRecipe = dotG4 . ((MakeFile.refVar "ANTLR4" +++ "-Dlanguage=" ++ MakeFile.refVar "DLANGUAGE" +++ MakeFile.refVar "OTHER_FLAGS") +++) . refVarWithPrefix

      rmFileRecipe refVar ext = "rm -f" +++ refVarWithPrefix refVar ++ ext

      makefileRules =  vcat $ makeRules
        [ (".PHONY", ["all", "clean-g4", "remove"], [])
        , ("all", [langRef], [])
        , ("lexer", [dotG4 $ refVarWithPrefix "LEXER_NAME"], [genAntlrRecipe "LEXER_NAME"])
        , ("parser", [dotG4 $ refVarWithPrefix "PARSER_NAME"], [genAntlrRecipe "PARSER_NAME"])
        , (langRef, ["lexer", "parser"], [])
        , ("clean-g4", [],
          [ rmFileRecipe "LEXER_NAME" ".interp"
          , rmFileRecipe "LEXER_NAME" ".tokens"
          , rmFileRecipe "PARSER_NAME" ".interp"
          , rmFileRecipe "PARSER_NAME" ".tokens"
          ])
        , ("remove", [], ["rm -rf" +++ langRef])
        ]

      makefileContent _ = vcat [makefileVars, "", makefileRules, ""]

mkAntlrComment :: String -> String
mkAntlrComment = ("// -*- ANTLRv4 -*- " ++)

parseAntlrTarget :: AntlrTarget -> String
parseAntlrTarget Java = "Java"
parseAntlrTarget CPP = "Cpp"
parseAntlrTarget CSharp = "CSharp"
parseAntlrTarget JS = "JavaScript"
parseAntlrTarget TS = "TypeScript"
parseAntlrTarget Dart = "Dart"
parseAntlrTarget Python3 = "Python3"
parseAntlrTarget PHP = "PHP"
parseAntlrTarget Go = "Go"
parseAntlrTarget Swift = "Swift"
