{-
    BNF Converter: C++ Main file
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer
    Copyright (C) 2020  Andreas Abel

    Modified from CPPTop to BNFC.Backend.CPP.STL 2006 by Aarne Ranta.

-}

module BNFC.Backend.CPP.STL (makeCppStl,) where

import Data.Foldable (toList)
import qualified Data.Map as Map

import BNFC.Utils
import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base
import BNFC.Backend.C            ( bufferH, bufferC, comment, testfileHeader )
import BNFC.Backend.C.CFtoBisonC ( cf2Bison )
import BNFC.Backend.C.CFtoFlexC  ( cf2flex, ParserMode(..) )
import BNFC.Backend.CPP.Common   ( commentWithEmacsModeHint )
import BNFC.Backend.CPP.Makefile
import BNFC.Backend.CPP.STL.CFtoSTLAbs
import BNFC.Backend.CPP.STL.CFtoCVisitSkelSTL
import BNFC.Backend.CPP.PrettyPrinter
import BNFC.Backend.CPP.STL.STLUtils
import qualified BNFC.Backend.Common.Makefile as Makefile

makeCppStl :: SharedOptions -> CF -> MkFiles ()
makeCppStl opts cf = do
    let (hfile, cfile) = cf2CPPAbs (linenumbers opts) (inPackage opts) name cf
    mkCppFile "Absyn.H" hfile
    mkCppFile "Absyn.C" cfile
    mkCppFile "Buffer.H" bufferH
    mkCppFile "Buffer.C" $ bufferC "Buffer.H"
    let (flex, env) = cf2flex parserMode cf
    mkCppFileWithHint (name ++ ".l") flex
    mkCppFileWithHint (name ++ ".y") $ cf2Bison (linenumbers opts) parserMode cf env
    mkCppFile "Parser.H" $
      mkHeaderFile (inPackage opts) cf (allParserCats cf) (toList $ allEntryPoints cf) (Map.elems env)
    mkCppFile "ParserError.H" $ printParseErrHeader (inPackage opts)
    let (skelH, skelC) = cf2CVisitSkel True (inPackage opts) cf
    mkCppFile "Skeleton.H" skelH
    mkCppFile "Skeleton.C" skelC
    let (prinH, prinC) = cf2CPPPrinter True (inPackage opts) cf
    mkCppFile "Printer.H" prinH
    mkCppFile "Printer.C" prinC
    mkCppFile "Test.C" (cpptest (inPackage opts) cf)
    Makefile.mkMakefile opts $ makefile prefix name
  where
    name :: String
    name = lang opts
    -- The prefix is a string used by flex and bison
    -- that is prepended to generated function names.
    -- It should be a valid C identifier.
    prefix :: String
    prefix = snakeCase_ name ++ "_"
    parserMode :: ParserMode
    parserMode = CppParser (inPackage opts) prefix
    mkCppFile         x = mkfile x comment
    mkCppFileWithHint x = mkfile x commentWithEmacsModeHint

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
cpptest inPackage cf = unlines $ concat
  [ testfileHeader
  , [ "",
    "#include <cstdio>",
    "#include <string>",
    "#include <iostream>",
    "#include <memory>",
    "#include \"Parser.H\"",
    "#include \"Printer.H\"",
    "#include \"Absyn.H\"",
    "#include \"ParserError.H\"",
    "",
    "void usage() {",
    "  printf(\"usage: Call with one of the following argument " ++
      "combinations:\\n\");",
    "  printf(\"\\t--help\\t\\tDisplay this help message.\\n\");",
    "  printf(\"\\t(no arguments)\\tParse stdin verbosely.\\n\");",
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
    "     std::cerr << \"Parse error on line \" << e.getLine() << \"\\n\"; ",
    "  }",
    "  if (parse_tree)",
    "  {",
    "    printf(\"\\nParse Successful!\\n\");",
    "    if (!quiet) {",
    "      printf(\"\\n[Abstract Syntax]\\n\");",
    "      " ++ scope ++ "std::unique_ptr<ShowAbsyn> " ++ scope ++ "s(new ShowAbsyn());",
    "      printf(\"%s\\n\\n\", s->show(parse_tree));",
    "      printf(\"[Linearized Tree]\\n\");",
    "      " ++ scope ++ "std::unique_ptr<PrintAbsyn> " ++ scope ++ "p(new PrintAbsyn());",
    "      printf(\"%s\\n\\n\", p->print(parse_tree));",
    "    }",
    "    delete(parse_tree);",
    "    return 0;",
    "  }",
    "  return 1;",
    "}",
    ""
    ]
  ]
  where
   cat = firstEntry cf
   dat = identCat $ normCat cat
   def = identCat cat
   scope = nsScope inPackage

mkHeaderFile inPackage _cf _cats eps _env = unlines $ concat
  [ [ "#ifndef " ++ hdef
    , "#define " ++ hdef
    , ""
    , "#include<vector>"
    , "#include<string>"
    , "#include<cstdio>"
    , "#include \"Absyn.H\""
    , ""
    , nsStart inPackage
    ]
  , concatMap mkFuncs eps
  , [ nsEnd inPackage
    , ""
    , "#endif"
    ]
  ]
  where
  hdef = nsDefine inPackage "PARSER_HEADER_FILE"
  mkFuncs s =
    [ identCat (normCat s) ++ "*" +++ "p" ++ identCat s ++ "(FILE *inp);"
    , identCat (normCat s) ++ "*" +++ "ps" ++ identCat s ++ "(const char *str);"
    ]
