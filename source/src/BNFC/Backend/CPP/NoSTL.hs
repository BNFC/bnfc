{-
    BNF Converter: C++ Main file
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer
-}

module BNFC.Backend.CPP.NoSTL (makeCppNoStl) where

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
import BNFC.Backend.CPP.NoSTL.CFtoCPPAbsAnsi
import BNFC.Backend.CPP.STL.CFtoCVisitSkelSTL
import BNFC.Backend.CPP.PrettyPrinter
import qualified BNFC.Backend.Common.Makefile as Makefile

makeCppNoStl :: SharedOptions -> CF -> MkFiles ()
makeCppNoStl opts cf = do
    let (hfile, cfile) = cf2CPPAbs name cf
    mkCppFile "Absyn.H" hfile
    mkCppFile "Absyn.C" cfile
    mkCppFile "Buffer.H" bufferH
    mkCppFile "Buffer.C" $ bufferC "Buffer.H"
    let (flex, env) = cf2flex parserMode cf
    mkCppFileWithHint (name ++ ".l") flex
    mkCppFileWithHint (name ++ ".y") $ cf2Bison (linenumbers opts) parserMode cf env
    mkCppFile "Parser.H" $
      mkHeaderFile cf (allParserCats cf) (toList $ allEntryPoints cf) (Map.elems env)
    let (skelH, skelC) = cf2CVisitSkel False Nothing cf
    mkCppFile "Skeleton.H" skelH
    mkCppFile "Skeleton.C" skelC
    let (prinH, prinC) = cf2CPPPrinter False Nothing cf
    mkCppFile "Printer.H" prinH
    mkCppFile "Printer.C" prinC
    mkCppFile "Test.C" (cpptest cf)
    Makefile.mkMakefile opts $ makefile prefix name compileOpt
  where
    name :: String
    name = lang opts
    -- The prefix is a string used by flex and bison
    -- that is prepended to generated function names.
    -- It should be a valid C identifier.
    prefix :: String
    prefix = snakeCase_ name ++ "_"
    compileOpt :: String
    compileOpt = "--ansi"
    parserMode :: ParserMode
    parserMode = CParser True prefix
    mkCppFile         x = mkfile x comment
    mkCppFileWithHint x = mkfile x commentWithEmacsModeHint

cpptest :: CF -> String
cpptest cf = unlines $ concat
  [ testfileHeader
  , [ "",
    "#include <stdio.h>",
    "#include <string.h>",
    "#include \"Parser.H\"",
    "#include \"Printer.H\"",
    "#include \"Absyn.H\"",
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
    "  " ++ dat ++ " *parse_tree = p" ++ def ++ "(input);",
    "  if (parse_tree)",
    "  {",
    "    printf(\"\\nParse Successful!\\n\");",
    "    if (!quiet) {",
    "      printf(\"\\n[Abstract Syntax]\\n\");",
    "      ShowAbsyn *s = new ShowAbsyn();",
    "      printf(\"%s\\n\\n\", s->show(parse_tree));",
    "      printf(\"[Linearized Tree]\\n\");",
    "      PrintAbsyn *p = new PrintAbsyn();",
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

mkHeaderFile _cf _cats eps _env = unlines $ concat
  [ [ "#ifndef PARSER_HEADER_FILE"
    , "#define PARSER_HEADER_FILE"
    , ""
    , "#include <stdio.h>"
    , "#include \"Absyn.H\""
    , ""
    ]
  , map mkFunc eps
  , [ ""
    , "#endif"
    ]
  ]
  where
  mkFunc s = identCat (normCat s) ++ "*" +++ "p" ++ identCat s ++ "(FILE *inp);"
