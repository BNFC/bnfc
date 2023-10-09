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
import BNFC.Backend.Antlr.Utils (getAntlrFlags, dotG4, parseAntlrTarget)
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

      antlrFlags = getAntlrFlags opts

      langRef = MakeFile.refVar "LANG"

      lexerVarName = "LEXER_FILENAME"
      lexerFilename = langRef ++ "Lexer"

      parserVarName = "PARSER_FILENAME"
      parserFilename = langRef ++ "Parser"

      prefix = "PREFIXED_"
      prefixedLexerVarName = prefix ++ lexerVarName
      prefixedParserVarName = prefix ++ parserVarName

      makefileVars = vcat $ makeVars
        [ ("LANG", lang)
        , (lexerVarName, lexerFilename)
        , (parserVarName, parserFilename)
        , (prefixedLexerVarName, langRef </> MakeFile.refVar lexerVarName)
        , (prefixedParserVarName, langRef </> MakeFile.refVar parserVarName)
        , ("ANTLR4", "java org.antlr.v4.Tool")
        , ("DLANGUAGE", parseAntlrTarget dLanguage)
        , ("OTHER_FLAGS", antlrFlags)
        ]

      genAntlrRecipe = dotG4 . ((MakeFile.refVar "ANTLR4" +++ "-Dlanguage=" ++ MakeFile.refVar "DLANGUAGE" +++ MakeFile.refVar "OTHER_FLAGS") +++) . MakeFile.refVar

      rmFileRecipe refVar ext = "rm -f" +++ MakeFile.refVar refVar ++ ext

      makefileRules =  vcat $ makeRules
        [ (".PHONY", ["all", "clean-g4", "remove"], [])
        , ("all", [langRef], [])
        , ("lexer", [dotG4 $ MakeFile.refVar prefixedLexerVarName], [genAntlrRecipe prefixedLexerVarName])
        , ("parser", [dotG4 $ MakeFile.refVar prefixedParserVarName], [genAntlrRecipe prefixedParserVarName])
        , (langRef, ["lexer", "parser"], [])
        , ("clean-g4", [],
          [ rmFileRecipe prefixedLexerVarName ".interp"
          , rmFileRecipe prefixedLexerVarName ".tokens"
          , rmFileRecipe prefixedParserVarName ".interp"
          , rmFileRecipe prefixedParserVarName ".tokens"
          ])
        , ("remove", [], ["rm -rf" +++ langRef])
        ]

      makefileContent _ = vcat [makefileVars, "", makefileRules, ""]

mkAntlrComment :: String -> String
mkAntlrComment = ("// -*- ANTLRv4 -*- " ++)
