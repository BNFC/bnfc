{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Antlr ( makeAntlr ) where

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator)

import BNFC.Utils
import BNFC.CF
import BNFC.Options as Options
import BNFC.Backend.Base
import BNFC.Backend.Antlr.CFtoAntlr4Lexer
import BNFC.Backend.Antlr.CFtoAntlr4Parser

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
    where
      lexerFun = cf2AntlrLex
      parserFun = cf2AntlrParse
      pkg = mkName [] CamelCase lang
      pkgToDir = replace '.' pathSeparator
      mkG4Name name = lang ++ name ++ ".g4"

mkAntlrComment :: String -> String
mkAntlrComment = ("// -*- ANTLRv4 -*- " ++)
