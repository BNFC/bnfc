{-
    BNF Converter: C++ Main file
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer

    Modified from CPPTop to BNFC.Backend.CPP.STL 2006 by Aarne Ranta.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

module BNFC.Backend.CPP.STL (makeCppStl,) where

import Data.Char
import Data.List (nub)
import qualified Data.Map as Map

import BNFC.Utils
import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base
import BNFC.Backend.C.CFtoBisonC (unionBuiltinTokens)
import BNFC.Backend.CPP.Makefile
import BNFC.Backend.CPP.STL.CFtoSTLAbs
import BNFC.Backend.CPP.NoSTL.CFtoFlex
import BNFC.Backend.CPP.STL.CFtoBisonSTL
import BNFC.Backend.CPP.STL.CFtoCVisitSkelSTL
import BNFC.Backend.CPP.PrettyPrinter
import BNFC.Backend.CPP.STL.STLUtils
import qualified BNFC.Backend.Common.Makefile as Makefile

makeCppStl :: SharedOptions -> CF -> MkFiles ()
makeCppStl opts cf = do
    let (hfile, cfile) = cf2CPPAbs (linenumbers opts) (inPackage opts) name cf
    mkfile "Absyn.H" hfile
    mkfile "Absyn.C" cfile
    let (flex, env) = cf2flex (inPackage opts) name cf
    mkfile (name ++ ".l") flex
    let bison = cf2Bison (linenumbers opts) (inPackage opts) name cf env
    mkfile (name ++ ".y") bison
    let header = mkHeaderFile (inPackage opts) cf (allParserCats cf) (allEntryPoints cf) (Map.elems env)
    mkfile "Parser.H" header
    mkfile "ParserError.H" $ printParseErrHeader (inPackage opts)
    let (skelH, skelC) = cf2CVisitSkel True (inPackage opts) cf
    mkfile "Skeleton.H" skelH
    mkfile "Skeleton.C" skelC
    let (prinH, prinC) = cf2CPPPrinter True (inPackage opts) cf
    mkfile "Printer.H" prinH
    mkfile "Printer.C" prinC
    mkfile "Test.C" (cpptest (inPackage opts) cf)
    Makefile.mkMakefile opts $ makefile name
  where name = lang opts

printParseErrHeader :: Maybe String -> String
printParseErrHeader inPackage =
  unlines
  [
     " #pragma once "
     , " #include <string>"
     , " #include <stdexcept>"
     , ""
     , nsStart inPackage
     , " class parse_error : public std::runtime_error"
     , " {"
     , " public:"
     , "     parse_error(int line, std::string str)"
     , "         : std::runtime_error(str)"
     , "         , m_line(line) {}"
     , "     int getLine() {"
     , "         return m_line;"
     , "     } "
     , " private:"
     , "     int m_line;"
     , " }; "
     , nsEnd inPackage
     ]

cpptest :: Maybe String -> CF -> String
cpptest inPackage cf =
  unlines
   [
    "/*** Compiler Front-End Test automatically generated by the BNF Converter ***/",
    "/*                                                                          */",
    "/* This test will parse a file, print the abstract syntax tree, and then    */",
    "/* pretty-print the result.                                                 */",
    "/*                                                                          */",
    "/****************************************************************************/",
    "#include <cstdio>",
    "#include <string>",
    "#include <iostream>",
    "#include \"Parser.H\"",
    "#include \"Printer.H\"",
    "#include \"Absyn.H\"",
    "#include \"ParserError.H\"",
    "",
    "void usage() {",
    "  printf(\"usage: Call with one of the following argument " ++
      "combinations:\\n\");",
    "  printf(\"\\t--help\\t\\tDisplay this help message.\\n\");",
    "  printf(\"\\t(no arguments)\tParse stdin verbosely.\\n\");",
    "  printf(\"\\t(files)\\t\\tParse content of files verbosely.\\n\");",
    "  printf(\"\\t-s (files)\\tSilent mode. Parse content of files " ++
      "silently.\\n\");",
    "}",
    "",
    "int main(int argc, char ** argv)",
    "{",
    "  FILE *input;",
    "  int quiet = 0;",
    "  char *filename = NULL;",
    "",
    "  if (argc > 1) {",
    "    if (strcmp(argv[1], \"-s\") == 0) {",
    "      quiet = 1;",
    "      if (argc > 2) {",
    "        filename = argv[2];",
    "      } else {",
    "        input = stdin;",
    "      }",
    "    } else {",
    "      filename = argv[1];",
    "    }",
    "  }",
    "",
    "  if (filename) {",
    "    input = fopen(filename, \"r\");",
    "    if (!input) {",
    "      usage();",
    "      exit(1);",
    "    }",
    "  } else input = stdin;",
    "  /* The default entry point is used. For other options see Parser.H */",
    "  " ++ scope ++ dat ++ " *parse_tree = NULL;",
    "  try { ",
    "  parse_tree = " ++ scope ++ "p" ++ def ++ "(input);",
    "  } catch( " ++ scope ++ "parse_error &e) {",
    "     std::cerr << \"Parse error on line \" << e.getLine(); ",
    "  }",
    "  if (parse_tree)",
    "  {",
    "    printf(\"\\nParse Successful!\\n\");",
    "    if (!quiet) {",
    "      printf(\"\\n[Abstract Syntax]\\n\");",
    "      " ++ scope ++ "ShowAbsyn *s = new " ++ scope ++ "ShowAbsyn();",
    "      printf(\"%s\\n\\n\", s->show(parse_tree));",
    "      printf(\"[Linearized Tree]\\n\");",
    "      " ++ scope ++ "PrintAbsyn *p = new " ++ scope ++ "PrintAbsyn();",
    "      printf(\"%s\\n\\n\", p->print(parse_tree));",
    "    }",
    "    return 0;",
    "  }",
    "  return 1;",
    "}",
    ""
   ]
  where
   cat = head $ allEntryPoints cf
   dat = identCat $ normCat cat
   def = identCat cat
   scope = nsScope inPackage

mkHeaderFile inPackage cf cats eps env = unlines $ concat
  [ [ "#ifndef " ++ hdef
    , "#define " ++ hdef
    , ""
    , "#include<vector>"
    , "#include<string>"
    , ""
    , nsStart inPackage
    ]
  , map mkForwardDec $ nub $ map normCat cats
  , [ "typedef union"
    , "{"
    ]
  , map ("  " ++) unionBuiltinTokens
  , concatMap mkVar cats
  , [ "} YYSTYPE;"
    , ""
    ]
  , concatMap mkFuncs eps
  , [ nsEnd inPackage
    , ""
    , "#define " ++ nsDefine inPackage "_ERROR_" ++ " 258"
    , mkDefines (259 :: Int) env
    , "extern " ++ nsScope inPackage ++ "YYSTYPE " ++ nsString inPackage ++ "yylval;"
    , ""
    , "#endif"
    ]
  ]
  where
  hdef = nsDefine inPackage "PARSER_HEADER_FILE"
  mkForwardDec s = "class " ++ identCat s ++ ";"
  mkVar s | normCat s == s = [ "  " ++ identCat s ++"*" +++ map toLower (identCat s) ++ "_;" ]
  mkVar _ = []
  mkDefines n [] = mkString n
  mkDefines n (s:ss) = "#define " ++ s +++ show n ++ "\n" ++ mkDefines (n+1) ss -- "nsDefine inPackage s" not needed (see cf2flex::makeSymEnv)
  mkString n =  if isUsedCat cf (TokenCat catString)
   then ("#define " ++ nsDefine inPackage "_STRING_ " ++ show n ++ "\n") ++ mkChar (n+1)
   else mkChar n
  mkChar n =  if isUsedCat cf (TokenCat catChar)
   then ("#define " ++ nsDefine inPackage "_CHAR_ " ++ show n ++ "\n") ++ mkInteger (n+1)
   else mkInteger n
  mkInteger n =  if isUsedCat cf (TokenCat catInteger)
   then ("#define " ++ nsDefine inPackage "_INTEGER_ " ++ show n ++ "\n") ++ mkDouble (n+1)
   else mkDouble n
  mkDouble n =  if isUsedCat cf (TokenCat catDouble)
   then ("#define " ++ nsDefine inPackage "_DOUBLE_ " ++ show n ++ "\n") ++ mkIdent(n+1)
   else mkIdent n
  mkIdent n =  if isUsedCat cf (TokenCat catIdent)
   then "#define " ++ nsDefine inPackage "_IDENT_ " ++ show n ++ "\n"
   else ""
  mkFuncs s =
    [ identCat (normCat s) ++ "*" +++ "p" ++ identCat s ++ "(FILE *inp);"
    , identCat (normCat s) ++ "*" +++ "p" ++ identCat s ++ "(const char *str);"
    ]
