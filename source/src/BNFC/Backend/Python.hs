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
import BNFC.Backend.Python.CFtoPyPrettyPrinter (cf2PyPretty)
import BNFC.Backend.Python.CFtoPySkele (cf2PySkele)
import BNFC.Backend.Python.PyHelpers
import BNFC.PrettyPrint
import Data.Char  (toLower, isLetter)
import qualified BNFC.Backend.Common.Makefile as Makefile


-- | Entrypoint for BNFC to use the Python backend.
makePython :: SharedOptions -> CF -> MkFiles ()
makePython opts cf = do
    let pkgName =  "bnfcPyGen" ++ filter isLetter name
    let (parsingDefs, abstractClasses) = cf2PyAbs pkgName cf
    let prettyPrinter = cf2PyPretty pkgName cf
    let skeletonCode = cf2PySkele pkgName cf
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
      [ "@echo \"Doing nothing: No compilation of the parser needed.\"" ]
  ,  Makefile.mkRule "clean" []
      [ "rm -f parser.out parsetab.py" ]
  , Makefile.mkRule "distclean" [ "vclean" ] []
  , Makefile.mkRule "vclean" []
      [ "rm -f " ++ unwords
        [ 
          pkgName ++ "/ParsingDefs.py",
          pkgName ++ "/Absyn.py",
          pkgName ++ "/PrettyPrinter.py",
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
  [ "import sys"
  , "from " ++ pkgName ++ ".ParsingDefs import *"
  , "from " ++ pkgName ++ ".PrettyPrinter import printAST, lin, renderC"
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
  , "def onError(e):"
  , "    print(e)"
  , "    print('Parse failed')"
  , "    quit(1)"
  , ""
  , "# Creates the Lark parser with the given grammar. By default to the first"
  , "# entrypoint. Other entrypoints exist in ParsingDefs.py."
  , "parser = Lark(grammar, start='" ++ defaultEntrypoint ++ "', parser='lalr', lexer='basic', transformer=TreeTransformer())"
  , ""
  , "# By default the first entrypoint is used. See ParsingDefs.py for alternatives."
  , "ast = parser.parse(inp, on_error=onError)"
  , "if ast:"
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
    defaultEntrypoint = map toLower 
      ((translateToList . show . firstEntry) cf)


