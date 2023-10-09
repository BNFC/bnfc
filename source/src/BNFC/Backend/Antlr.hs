{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Antlr ( makeAntlr ) where

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator, (<.>))
import Text.PrettyPrint.HughesPJ (vcat)

import BNFC.Utils
import BNFC.CF
import BNFC.Options as Options
import BNFC.Backend.Base
import BNFC.Backend.Antlr.CFtoAntlr4Lexer
import BNFC.Backend.Antlr.CFtoAntlr4Parser
import BNFC.Backend.Common.Makefile as MakeFile

makeAntlr :: SharedOptions -> CF -> MkFiles ()
makeAntlr Options{..} cf = do
  let packageBase = maybe id (+.+) inPackage pkg
      dirBase = pkgToDir packageBase

  let (lex, env) = lexerFun packageBase cf
    -- Where the lexer file is created. lex is the content!
  mkfile (dirBase </> mkG4Name "Lexer") mkAntlrComment lex
  -- liftIO $ putStrLn $ "   (Tested with" +++ toolname lexmake
  --                                       +++ toolversion lexmake  ++ ")"
  let parserContent = parserFun packageBase cf linenumbers env
  mkfile (dirBase </> mkG4Name "Parser") mkAntlrComment parserContent

  MakeFile.mkMakefile optMake makefileContent
    where
      lexerFun = cf2AntlrLex
      parserFun = cf2AntlrParse
      pkg = mkName [] CamelCase lang
      pkgToDir = replace '.' pathSeparator
      mkG4Name name = lang ++ name <.> "g4"

      makeVars x = [MakeFile.mkVar n v | (n,v) <- x]
      makeRules x = [MakeFile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]

      otherFlags = unwords $ getFlags [("no-listener", not listener), ("visitor", visitor), ("Werror", wError)]

      langRef = MakeFile.refVar "LANG"

      makefileVars = vcat $ makeVars
        [ ("LANG", lang)
        , ("LEXER_NAME", langRef ++ "Lexer")
        , ("PARSER_NAME", langRef ++ "Parser")
        , ("ANTLR4", "java org.antlr.v4.Tool")
        , ("DLANGUAGE", parseAntlrTarget dLanguage)
        , ("OTHER_FLAGS", otherFlags)
        ]

      refVarWithPrefix :: String -> String
      refVarWithPrefix refVar = langRef </> MakeFile.refVar refVar

      rmFile :: String -> String -> String
      rmFile refVar ext = "rm -f" +++ refVarWithPrefix refVar ++ ext

      makefileRules =  vcat $ makeRules
        [ (".PHONY", ["all", "clean-g4", "remove"], [])
        , ("all", [langRef], [])
        , ("lexer", [refVarWithPrefix "LEXER_NAME" <.> "g4"], [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=" ++ MakeFile.refVar "DLANGUAGE" +++ MakeFile.refVar "OTHER_FLAGS" +++ refVarWithPrefix "LEXER_NAME" <.> "g4"])
        , ("parser", [refVarWithPrefix "PARSER_NAME" <.> "g4"], [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=" ++ MakeFile.refVar "DLANGUAGE" +++ MakeFile.refVar "OTHER_FLAGS" +++ refVarWithPrefix "PARSER_NAME" <.> "g4"])
        , (langRef, ["lexer", "parser"], [])
        , ("clean-g4", [],
          [ rmFile "LEXER_NAME" ".interp"
          , rmFile "LEXER_NAME" ".tokens"
          , rmFile "PARSER_NAME" ".interp"
          , rmFile "PARSER_NAME" ".tokens"
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

getFlags :: [(String, Bool)] -> [String]
getFlags (x : xs) = case x of
  (flag, True) -> ("-" ++ flag) : getFlags xs
  (_, False)   -> getFlags xs

getFlags [] = []
