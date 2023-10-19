{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Antlr ( makeAntlr ) where

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator, (<.>))
import Text.PrettyPrint.HughesPJ (vcat)
import Data.Bifunctor (second)
import Data.Char (toUpper, toLower)

import BNFC.Utils
    ( NameStyle(CamelCase, SnakeCase),
      mkName,
      replace,
      (+.+),
      (+++),
      mkNames )
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

      generatedFilesVars = map (second (langRef </>)) $ getVars dLanguage pkg

      makefileVars = vcat $ makeVars $ 
        [ ("LANG", pkg)
        , (lexerVarName, lexerGrammarFile)
        , (parserVarName, parserGrammarFile)
        , ("ANTLR4", "java org.antlr.v4.Tool")
        , ("ANTLR_OPTIONS", getAntlrOptions opts)
        , ("DIRECT_OPTIONS", antlrOpts)
        ]
        ++ generatedFilesVars

      genAntlrRecipe = ((MakeFile.refVar "ANTLR4" +++ MakeFile.refVar "ANTLR_OPTIONS" +++ MakeFile.refVar "DIRECT_OPTIONS") +++) . MakeFile.refVar

      rmFile refVar = "rm -f" +++ MakeFile.refVar refVar

      antlrFiles = map (langRef </>)
        [ (mkName [] CamelCase $ pkg +++ "Lexer") <.> "interp"
        , (mkName [] CamelCase $ pkg +++ "Parser") <.> "interp"
        , (mkName [] CamelCase $ pkg +++ "Lexer") <.> "tokens"
        , (mkName [] CamelCase $ pkg +++ "Parser") <.> "tokens"
        ]

      makefileRules =  vcat $ makeRules
        [ (".PHONY", ["all", "clean-antlr", "remove"], [])
        , ("all", [langRef], [])
        , ("lexer", [MakeFile.refVar lexerVarName], [genAntlrRecipe lexerVarName])
        , ("parser", [MakeFile.refVar parserVarName], [genAntlrRecipe parserVarName])
        , (langRef, ["lexer", "parser"], [])
        , ("clean-antlr", [],
          map rmFile targetLanguageFiles
          ++
          map ("rm -f" +++) antlrFiles )
        , ("remove", [], ["rm -rf" +++ langRef])
        ]

      makefileContent _ = vcat [makefileVars, "", makefileRules]

mkAntlrComment :: String -> String
mkAntlrComment = ("// ANTLRv4 " ++)

targetLanguageFiles :: [String]
targetLanguageFiles = ["LEXER", "PARSER", "LISTENER", "VISITOR", "BASE_LISTENER", "BASE_VISITOR"]

getVars :: AntlrTarget -> [Char] -> [(String, FilePath)]
getVars target lang = zip targetLanguageFiles files
  where
    files = map (<.> ext) names
    names = mkNames [] namestyle
      [ filename "lexer"
      , filename "parser"
      , filename "parser listener"
      , filename "parser visitor"
      , filename "parser base listener"
      , filename "parser base visitor"
      ]
    
    filename = case target of
      Go -> (toLowerCase lang ++)
      _  -> (lang +++)

    namestyle = case target of
      Go -> SnakeCase
      _ -> CamelCase

    ext = getExt target

-- file ext. depending on target
getExt :: AntlrTarget -> String
getExt Java    = "java"
getExt CPP     = "cpp"
getExt CSharp  = "cs"
getExt JS      = "js"
getExt TS      = "ts"
getExt Dart    = "dart"
getExt Python3 = "py"
getExt PHP     = "php"
getExt Go      = "go"
getExt Swift   = "swift"

toUppercase :: [Char] -> [Char]
toUppercase = map toUpper
toLowerCase = map toLower
