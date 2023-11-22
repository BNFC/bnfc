{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Antlr ( makeAntlr ) where

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator, (<.>))
import Text.PrettyPrint.HughesPJ (vcat)

import BNFC.Utils
    ( NameStyle(CamelCase),
      mkName,
      replace,
      (+.+),
      (+++) )
import BNFC.CF
import BNFC.Options as Options
import BNFC.Backend.Base
import BNFC.Backend.Antlr.CFtoAntlr4Lexer
import BNFC.Backend.Antlr.CFtoAntlr4Parser
import BNFC.Backend.Antlr.Utils (dotG4, getAntlrOptions)
import BNFC.Backend.Common.Makefile as MakeFile
    ( mkMakefile, mkVar, mkRule, refVar )

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
      mkG4Filename = dotG4 . (pkg ++)

      makeVars x = [MakeFile.mkVar n v | (n,v) <- x]
      makeRules x = [MakeFile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]

      langRef = MakeFile.refVar "LANG"

      lexerVarName = "LEXER_GRAMMAR_FILENAME"
      lexerGrammarFile = (langRef </>) . dotG4 $ langRef ++ "Lexer"

      parserVarName = "PARSER_GRAMMAR_FILENAME"
      parserGrammarFile = (langRef </>) . dotG4 $ langRef ++ "Parser"

      makefileVars = vcat $ makeVars 
        [ ("LANG", pkg)
        , (lexerVarName, lexerGrammarFile)
        , (parserVarName, parserGrammarFile)
        , ("ANTLR4", "java org.antlr.v4.Tool")
        , ("ANTLR_OPTIONS", getAntlrOptions opts)
        , ("DIRECT_OPTIONS", antlrOpts)
        ]

      genAntlrRecipe = ((MakeFile.refVar "ANTLR4" +++ MakeFile.refVar "ANTLR_OPTIONS" +++ MakeFile.refVar "DIRECT_OPTIONS") +++) . MakeFile.refVar

      antlrFiles = map (langRef </>)
        [ mkName [] CamelCase (pkg +++ "Lexer") <.> "interp"
        , mkName [] CamelCase (pkg +++ "Parser") <.> "interp"
        , mkName [] CamelCase (pkg +++ "Lexer") <.> "tokens"
        , mkName [] CamelCase (pkg +++ "Parser") <.> "tokens"
        ]

      makefileRules =  vcat $ makeRules
        [ (".PHONY", ["all", "clean-antlr", "remove"], [])
        , ("all", [langRef], [])
        , ("lexer", [MakeFile.refVar lexerVarName], [genAntlrRecipe lexerVarName])
        , ("parser", [MakeFile.refVar parserVarName], [genAntlrRecipe parserVarName])
        , (langRef, ["lexer", "parser"], [])
        , ("clean-antlr", [],
          map ("rm -f" +++) antlrFiles )
        , ("remove", [], ["rm -rf" +++ langRef])
        ]

      makefileContent _ = vcat [makefileVars, "", makefileRules]

mkAntlrComment :: String -> String
mkAntlrComment = ("// ANTLRv4 " ++)
