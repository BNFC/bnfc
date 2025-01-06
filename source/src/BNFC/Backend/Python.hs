{-
    BNF Converter: Python main file
    Copyright (C) 2004  Author:  Bjorn Werner
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Python (makePython) where

import Prelude hiding ((<>))
import System.FilePath ((</>))
import BNFC.CF (CF, firstEntry)
import BNFC.Options (SharedOptions, optMake, lang)
import BNFC.Backend.Base (MkFiles, mkfile)
import BNFC.Backend.Python.CFtoPyAbs (cf2PyAbs)
import BNFC.Backend.Python.CFtoPyPrettyPrinter (cf2PyPretty)
import BNFC.Backend.Python.CFtoPySkele (cf2PySkele)
import BNFC.Backend.Python.PyHelpers
import BNFC.PrettyPrint
import Data.Char  (toLower, isLetter)
import qualified BNFC.Backend.Common.Makefile as Makefile

import BNFC.Backend.Python.CFtoAntlr4Lexer (cf2AntlrLex)
import BNFC.Backend.Python.CFtoAntlr4Parser (cf2AntlrParse)


-- | Entrypoint for BNFC to use the Python backend.
makePython :: SharedOptions -> CF -> MkFiles ()
makePython opts cf = do
    let pkgName =  "bnfcPyGen" ++ filteredName
    let abstractClasses = cf2PyAbs cf
    let prettyPrinter = cf2PyPretty pkgName cf
    let skeletonCode = cf2PySkele pkgName cf
    mkPyFile (pkgName ++ "/Absyn.py") abstractClasses
    mkPyFile (pkgName ++ "/PrettyPrinter.py") prettyPrinter
    mkPyFile "skele.py" skeletonCode
    mkPyFile "genTest.py" (pyTest pkgName filteredName cf)
    Makefile.mkMakefile (optMake opts) $ makefile pkgName filteredName (optMake opts)

    let (d, kwenv) = cf2AntlrLex filteredName cf
    mkAntlrFile (pkgName ++ "/" ++ filteredName ++ "Lexer.g4") d
    --cf2AntlrParse :: String -> String -> CF -> KeywordEnv -> String
    let p = cf2AntlrParse filteredName (pkgName ++ ".Absyn") cf kwenv
    mkAntlrFile (pkgName ++ "/" ++ filteredName ++ "Parser.g4") p
  where
    name :: String
    name = lang opts
    filteredName = filter isLetter name
    mkPyFile x = mkfile x comment
    mkAntlrFile x = mkfile x ("//" ++) -- "//" for comments

-- | A makefile with distclean and clean specifically for the testsuite. No
--   "all" is needed as bnfc has already generated the necessary Python files.
makefile :: String -> String -> Maybe String -> String -> Doc
makefile pkgName filteredName optMakefileName basename = vcat
  [
    Makefile.mkRule "all" []
      [ "java -jar $(ANTLR) -Dlanguage=Python3 " ++ pkgName ++ "/" ++ filteredName ++ "Lexer.g4"
      , "java -jar $(ANTLR) -Dlanguage=Python3 " ++ pkgName ++ "/" ++ filteredName ++ "Parser.g4" ]
  ,  Makefile.mkRule "clean" []
      [ "rm -f parser.out parsetab.py" ]
  , Makefile.mkRule "distclean" [ "vclean" ] []
  , Makefile.mkRule "vclean" []
      [ "rm -f " ++ unwords
        [ 
          pkgName ++ "/Absyn.py",
          pkgName ++ "/PrettyPrinter.py",
          pkgName ++ "/Absyn.py.bak",
          pkgName ++ "/PrettyPrinter.py.bak",
          pkgName ++ "/" ++ filteredName ++ "*",
          "skele.py",
          "genTest.py",
          "skele.py.bak",
          "genTest.py.bak"
        ],
        "rm -f " ++ pkgName ++ "/__pycache__/*.pyc",
        "rm -fd " ++ pkgName ++ "/__pycache__",
        "rmdir " ++ pkgName,
        "rm -f __pycache__/*.pyc",
        "rm -fd __pycache__",
        "rm -f " ++ makefileName,
        "rm -f " ++ makefileName ++ ".bak"
      ]
  ]
  where
    makefileName = case optMakefileName of
      Just s -> s
      Nothing -> "None" -- No makefile will be created.


-- | Put string into a comment.
comment :: String -> String
comment x = "# " ++ x


-- Produces the content for the testing file, genTest.py.
pyTest :: String -> String -> CF -> String
pyTest pkgName filteredName cf = unlines 
  [ "import sys"
  , "from " ++ pkgName ++ ".PrettyPrinter import printAST, lin, renderC"
  , "from antlr4 import *"
  , "from " ++ pkgName ++ "." ++ lexerName ++ " import " ++ lexerName
  , "from " ++ pkgName ++ "." ++ parserName ++ " import " ++ parserName
  , "from antlr4.error.ErrorListener import ErrorListener"
  , ""
  , "# Suggested input options:"
  , "# python3 genTest.py < sourcefile"
  , "# python3 genTest.py sourcefile inputfile (i.e. for interpreters)."
  , "inputFile = None"
  , "if len(sys.argv) > 1:"
  , "    f = open(sys.argv[1], 'r')"
  , "    inp = f.read()"
  , "    f.close()"
  , "    if len(sys.argv) > 2:"
  , "        inputFile = sys.argv[2]"
  , "else:"
  , "    inp = ''"
  , "    for line in sys.stdin:"
  , "        inp += line"
  , ""
  , "class ThrowingErrorListener(ErrorListener):"
  , "    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):"
  , "        raise Exception(f'Syntax error at line {line}, column {column}: {msg}')"
  , ""
  , "try:"
  , "    lexer = " ++ lexerName ++ "(InputStream(inp))"
  , "    lexer.removeErrorListeners()"
  , "    lexer.addErrorListener(ThrowingErrorListener())"
  , ""
  , "    stream = CommonTokenStream(lexer)"
  , "    parser = " ++ parserName ++ "(stream)"
  , "    parser.removeErrorListeners()"
  , "    parser.addErrorListener(ThrowingErrorListener())"
  , ""
  , "    tree = parser.start_" ++ defaultEntrypoint ++ "()"
  , "    ast = tree.result"
  , "    error = False"
  , "except Exception as e:"
  , "    print(e)"
  , "    error = True"
  , ""
  , "if not error and ast:"
  , "    print('Parse Successful!\\n')"
  , "    print('[Abstract Syntax]')"
  , "    print(printAST(ast))"
  , "    print('\\n[Linearized Tree]')"
  , "    linTree = lin(ast)"
  , "    print(renderC(linTree))"
  , "    print()"
  , "else:"
  , "    print('Parse failed')"
  , "    quit(1)"
  ]
  where
    lexerName = filteredName ++ "Lexer"
    parserName = filteredName ++ "Parser"
    defaultEntrypoint = (translateToList . show . firstEntry) cf


