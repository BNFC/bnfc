{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Python main file
    Copyright (C) 2004  Author:  Bjorn Werner
-}

module BNFC.Backend.Python (makePython) where

import Prelude hiding ((<>))
import System.FilePath ((</>))
import BNFC.CF (CF, firstEntry)
import BNFC.Options (SharedOptions, optMake, lang)
import BNFC.Backend.Base (MkFiles, mkfile)
import BNFC.Backend.Python.CFtoPyAbs (cf2PyAbs)
import BNFC.Backend.Python.CFtoPyLex (cf2PyLex)
import BNFC.Backend.Python.CFtoPyPrettyPrinter (cf2PyPretty)
import BNFC.Backend.Python.CFtoPySkele (cf2PySkele)
import BNFC.Backend.Python.PyHelpers

import BNFC.PrettyPrint -- For Doc
import qualified BNFC.Backend.Common.Makefile as Makefile

-- | Entrypoint for BNFC to use the Python backend.
makePython :: SharedOptions -> CF -> MkFiles ()
makePython opts cf = do
    let pkgName =  "bnfcPyGen" ++ name
    let (lexerDefs, tokensPly) = cf2PyLex cf
    let (parsingDefs, abstractClasses) = cf2PyAbs pkgName cf tokensPly
    let prettyPrinter = cf2PyPretty pkgName cf
    let skeletonCode = cf2PySkele pkgName cf
    mkPyFile (pkgName ++ "/LexTokens.py") lexerDefs
    mkPyFile (pkgName ++ "/ParsingDefs.py") parsingDefs
    mkPyFile (pkgName ++ "/Absyn.py") abstractClasses
    mkPyFile (pkgName ++ "/PrettyPrinter.py") prettyPrinter
    mkPyFile "skele.py" skeletonCode
    mkPyFile "genTest.py" (pyTest pkgName cf)
    Makefile.mkMakefile (optMake opts) $ makefile pkgName (optMake opts)
  where
    name :: String
    name = lang opts
    mkPyFile x = mkfile x comment


-- | A makefile with distclean and clean specifically for the testsuite. No
--   "all" is needed as bnfc has already generated the necessary Python files.
makefile :: String -> Maybe String -> String -> Doc
makefile pkgName optMakefileName basename = vcat
  [
    Makefile.mkRule "all" []
      [ " " ]
  ,  Makefile.mkRule "clean" []
      [ "rm -f parser.out parsetab.py" ]
  , Makefile.mkRule "distclean" [ "vclean" ] []
  , Makefile.mkRule "vclean" []
      [ "rm -f " ++ unwords
        [ 
          pkgName ++ "/LexTokens.py",
          pkgName ++ "/ParsingDefs.py",
          pkgName ++ "/Absyn.py",
          pkgName ++ "/PrettyPrinter.py",
          pkgName ++ "/LexTokens.py.bak",
          pkgName ++ "/ParsingDefs.py.bak",
          pkgName ++ "/Absyn.py.bak",
          pkgName ++ "/PrettyPrinter.py.bak",
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
        "rm -f parser.out parsetab.py",
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
pyTest :: String -> CF -> String
pyTest pkgName cf = unlines 
  [
    "from ply.lex import lex",
    "from ply.yacc import yacc",
    "import sys",
    "from " ++ pkgName ++ ".LexTokens import *",
    "from " ++ pkgName ++ ".ParsingDefs import *",
    "from " ++ pkgName ++ ".PrettyPrinter import *",
    "",
    "",
    "# Suggested input options:",
    "# python3.10 genTest.py < sourcefile",
    "# python3.10 genTest.py sourcefile inputfile (i.e. for interpreters).",
    "inputFile = None",
    "if len(sys.argv) > 1:",
    "\tf = open(sys.argv[1], 'r')",
    "\tinp = f.read()",
    "\tf.close()",
    "\tif len(sys.argv) > 2:",
    "\t\tinputFile = sys.argv[2]",
    "else:",
    "\tinp = ''",
    "\tfor line in sys.stdin:",
    "\t\tinp += line",
    "",
    "",
    "# Customizable error handling for the parsing",
    "def p_error(p: lex.LexToken):",
      "\tif p is None:",
        "\t\tprint('No rule could reduce the tokenized input')",
      "\telse:",
        "\t\tprint('line:', p.lineno, 'lexpos:', p.lexpos, f'Syntax error at {p.value!r}')",
        "\t\tp.lexer.syntaxError = True",
    "",
    "",
    "# By default the first entrypoint is used. See ParsingDefs.py for alternatives.",
    "lexer = lex.lex()",
    "parser = yacc(start=" ++ defaultEntry ++ ")",
    "lexer.syntaxError = False",
    "ast = parser.parse(inp, lexer=lexer)",
    "if ast and not lexer.syntaxError:",
      "\tprint('Parse Successful!\\n')",
      "\tprint('[Abstract Syntax]')",
      "\tprint(printAST(ast))",
      "\tprint('\\n[Linearized Tree]')",
      "\tlinTree = lin(ast)",
      "\tprint(renderC(linTree))",
      "\tprint()",
    "else:",
      "\tprint('Parse failed')",
      "\tquit(1)"
  ]
  where
    defaultEntry = (addCitationSigns . translateToList . show . firstEntry) cf

