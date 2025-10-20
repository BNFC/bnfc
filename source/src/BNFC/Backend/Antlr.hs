{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Antlr ( makeAntlr, makeAntlr', DirectoryOptions(..) ) where

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator, (<.>))
import Text.PrettyPrint.HughesPJ (vcat)
import Data.Maybe (fromMaybe)

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

data DirectoryOptions = DirectoryOptions
  { baseDirectory :: Maybe String
  , nameStyle :: Maybe NameStyle }

makeAntlr :: SharedOptions -> CF -> MkFiles ()
makeAntlr opts cf = makeAntlr' opts cf DirectoryOptions {
    baseDirectory=Nothing
  , nameStyle=Nothing }

makeAntlr' :: SharedOptions -> CF -> DirectoryOptions -> MkFiles ()
makeAntlr' opts@Options{..} cf DirectoryOptions{..} = do
  let packageBase = maybe id (+.+) inPackage pkg
      dirBase = fromMaybe (pkgToDir packageBase) baseDirectory

  let lexerName = mkFilename "Lexer"
      lexerFile = dotG4 lexerName
      (lex, env) = cf2AntlrLex lexerName cf
    -- Where the lexer file is created. lex is the content!
  mkfile (dirBase </> lexerFile) mkAntlrComment lex

  let parserName = mkFilename "Parser"
      parserFile = dotG4 parserName
      parserContent = cf2AntlrParse lexerName parserName cf linenumbers env
  mkfile (dirBase </> parserFile) mkAntlrComment parserContent

  MakeFile.mkMakefile optMake makefileContent
    where
      pkg = mkName [] (fromMaybe CamelCase nameStyle) lang
      pkgToDir = replace '.' pathSeparator
      mkFilename ending = mkName [] (fromMaybe CamelCase nameStyle) (pkg ++ ending)

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

      antlrFiles = 
        let ns = fromMaybe CamelCase nameStyle 
        in map (langRef </>)
          [ mkName [] ns (pkg +++ "Lexer") <.> "interp"
          , mkName [] ns (pkg +++ "Parser") <.> "interp"
          , mkName [] ns (pkg +++ "Lexer") <.> "tokens"
          , mkName [] ns (pkg +++ "Parser") <.> "tokens"
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
