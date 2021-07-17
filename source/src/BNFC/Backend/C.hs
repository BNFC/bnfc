{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: C Main file
    Copyright (C) 2004  Author:  Michael Pellauer
    Copyright (C) 2020  Andreas Abel
-}
module BNFC.Backend.C (makeC, bufferC, bufferH, comment, testfileHeader) where

import Prelude hiding ((<>))

import Data.Foldable (toList)
import qualified Data.Map as Map

import BNFC.Utils
import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base
import BNFC.Backend.C.CFtoCAbs
import BNFC.Backend.C.CFtoFlexC
import BNFC.Backend.C.CFtoBisonC
import BNFC.Backend.C.CFtoCSkel
import BNFC.Backend.C.CFtoCPrinter
import BNFC.PrettyPrint

import qualified BNFC.Backend.Common.Makefile as Makefile

makeC :: SharedOptions -> CF -> MkFiles ()
makeC opts cf = do
    let (hfile, cfile) = cf2CAbs (linenumbers opts) prefix cf
    mkCFile "Absyn.h" hfile
    mkCFile "Absyn.c" cfile
    mkCFile "Buffer.h" bufferH
    mkCFile "Buffer.c" $ bufferC "Buffer.h"
    let (flex, env) = cf2flex parserMode cf
    mkCFile (name ++ ".l") flex
    let bison = cf2Bison (linenumbers opts) parserMode cf env
    mkCFile (name ++ ".y") bison
    let header = mkHeaderFile (linenumbers opts) cf (Map.elems env)
    mkCFile "Parser.h" header
    let (skelH, skelC) = cf2CSkel cf
    mkCFile "Skeleton.h" skelH
    mkCFile "Skeleton.c" skelC
    let (prinH, prinC) = cf2CPrinter cf
    mkCFile "Printer.h" prinH
    mkCFile "Printer.c" prinC
    mkCFile "Test.c" (ctest cf)
    Makefile.mkMakefile opts (makefile name prefix)
  where
    name :: String
    name = lang opts
    -- The prefix is a string used by flex and bison
    -- that is prepended to generated function names.
    -- It should be a valid C identifier.
    prefix :: String
    prefix = snakeCase_ name ++ "_"
    parserMode :: ParserMode
    parserMode = CParser False prefix
    mkCFile x = mkfile x comment


makefile :: String -> String -> String -> Doc
makefile name prefix basename = vcat
    [ "CC = gcc -g"
    , "CCFLAGS = --ansi -W -Wall -Wno-unused-parameter -Wno-unused-function -Wno-unneeded-internal-declaration ${CC_OPTS}"
    -- The @#define _POSIX_C_SOURCE 200809L@ is now placed locally in
    -- the generated lexer.
    -- , "CCFLAGS = --ansi -W -Wall -Wno-unused-parameter -Wno-unused-function -Wno-unneeded-internal-declaration -D_POSIX_C_SOURCE=200809L ${CC_OPTS}"
    -- , "# Setting _POSIX_C_SOURCE to 200809L activates strdup in string.h."
    -- , "# strdup was not in the ISO C standard before 6/2019 (C2x), yet in POSIX 1003.1."
    -- , "# See https://en.cppreference.com/w/c/experimental/dynamic/strdup"
    , ""
    , "FLEX = flex"
    , "FLEX_OPTS = -P" <> text prefix
    , ""
    , "BISON = bison"
    , "BISON_OPTS = -t -p" <> text prefix
    , ""
    , "OBJS = Absyn.o Buffer.o Lexer.o Parser.o Printer.o"
    , ""
    , Makefile.mkRule ".PHONY" ["clean", "distclean"]
      []
    , Makefile.mkRule "all" [testName]
      []
    , Makefile.mkRule "clean" []
      -- peteg: don't nuke what we generated - move that to the "vclean" target.
      [ "rm -f *.o " ++ testName ++ " " ++ unwords
        [ name ++ e | e <- [".aux", ".log", ".pdf",".dvi", ".ps", ""]] ]
    , Makefile.mkRule "distclean" ["clean"]
      [ "rm -f " ++ unwords
        [ "Absyn.h", "Absyn.c"
        , "Bison.h"
        , "Buffer.h", "Buffer.c"
        , name ++ ".l", "Lexer.c"
        , name ++ ".y", "Parser.h", "Parser.c"
        , "Printer.c", "Printer.h"
        , "Skeleton.c", "Skeleton.h"
        , "Test.c"
        , basename, name ++ ".tex"
        ]
      ]
    , Makefile.mkRule testName ["${OBJS}", "Test.o"]
      [ "@echo \"Linking " ++ testName ++ "...\""
      , "${CC} ${OBJS} Test.o -o " ++ testName ]
    , Makefile.mkRule "Absyn.o" [ "Absyn.c", "Absyn.h"]
      [ "${CC} ${CCFLAGS} -c Absyn.c" ]
    , Makefile.mkRule "Buffer.o" [ "Buffer.c", "Buffer.h"]
      [ "${CC} ${CCFLAGS} -c Buffer.c" ]
    , Makefile.mkRule "Lexer.c" [ name ++ ".l" ]
      [ "${FLEX} ${FLEX_OPTS} -oLexer.c " ++ name ++ ".l" ]
    , Makefile.mkRule "Parser.c Bison.h" [ name ++ ".y" ]
      [ "${BISON} ${BISON_OPTS} " ++ name ++ ".y -o Parser.c" ]
    , Makefile.mkRule "Lexer.o" [ "Lexer.c", "Bison.h" ]
      [ "${CC} ${CCFLAGS} -c Lexer.c " ]
    , Makefile.mkRule "Parser.o" ["Parser.c", "Absyn.h", "Bison.h" ]
      [ "${CC} ${CCFLAGS} -c Parser.c" ]
    , Makefile.mkRule "Printer.o" [ "Printer.c", "Printer.h", "Absyn.h" ]
      [ "${CC} ${CCFLAGS} -c Printer.c" ]
    , Makefile.mkRule "Test.o" [ "Test.c", "Parser.h", "Printer.h", "Absyn.h" ]
      [ "${CC} ${CCFLAGS} -c Test.c" ]
    ]
  where testName = "Test" ++ name

-- | Put string into a block comment.
comment :: String -> String
comment x = unwords ["/*", x, "*/"]

-- | A heading comment for the generated parser test.
testfileHeader :: [String]
testfileHeader =
  [ "/************************* Compiler Front-End Test *************************/"
  , "/*                                                                         */"
  , "/*  This test will parse a file, print the abstract syntax tree, and then  */"
  , "/*  pretty-print the result.                                               */"
  , "/*                                                                         */"
  , "/***************************************************************************/"
  ]

-- | Generate a test program that parses stdin and prints the AST and it's
-- linearization
ctest :: CF -> String
ctest cf = unlines $ testfileHeader ++
   [
    "",
    "#include <stdio.h>",
    "#include <stdlib.h>",
    "#include <string.h>",
    "",
    "#include \"Parser.h\"",
    "#include \"Printer.h\"",
    "#include \"Absyn.h\"",
    "",
    "void usage(void) {",
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
    "  " ++ dat ++ " parse_tree;",
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
    "  }",
    "  else input = stdin;",
    "  /* The default entry point is used. For other options see Parser.h */",
    "  parse_tree = p" ++ def ++ "(input);",
    "  if (parse_tree)",
    "  {",
    "    printf(\"\\nParse Successful!\\n\");",
    "    if (!quiet) {",
    "      printf(\"\\n[Abstract Syntax]\\n\");",
    "      printf(\"%s\\n\\n\", show" ++ dat ++ "(parse_tree));",
    "      printf(\"[Linearized Tree]\\n\");",
    "      printf(\"%s\\n\\n\", print" ++ dat ++ "(parse_tree));",
    "    }",
    "    free_" ++ dat ++ "(parse_tree);",
    "    return 0;",
    "  }",
    "  return 1;",
    "}",
    ""
   ]
  where
  cat :: Cat
  cat = firstEntry cf
  def :: String
  def = identCat cat
  dat :: String
  dat = identCat . normCat $ cat

mkHeaderFile :: RecordPositions -> CF -> [String] -> String
mkHeaderFile _ cf _env = unlines $ concat
  [ [ "#ifndef PARSER_HEADER_FILE"
    , "#define PARSER_HEADER_FILE"
    , ""
    , "#include \"Absyn.h\""
    , ""
    ]
  -- Andreas, 2021-03-24
  -- Removed stuff that is now generated in Bison.h using the %defines pragma in the .y file.
  , concatMap mkFunc $ toList $ allEntryPoints cf
  , [ ""
    , "#endif"
    ]
  ]
  where
  -- Andreas, 2019-04-29, issue #210: generate parsers also for coercions
  mkFunc c =
    [ identCat (normCat c) ++ "  p" ++ identCat c ++ "(FILE *inp);"
    , identCat (normCat c) ++ " ps" ++ identCat c ++ "(const char *str);"
    ]


-- | A tiny buffer library for string buffers in the lexer.

bufferH :: String
bufferH = unlines
  [ "/* A dynamically allocated character buffer that grows as it is appended. */"
  , ""
  , "#ifndef BUFFER_HEADER"
  , "#define BUFFER_HEADER"
  , ""
  , "typedef struct buffer {"
  , "  char* chars;           /* Pointer to start of the buffer.        */"
  , "  unsigned int size;     /* Buffer size (>= 1).                    */"
  , "  unsigned int current;  /* Next free character position (< size). */"
  , "} * Buffer;"
  , ""
  , "/* External interface. */"
  , "/************************************************************************/"
  , ""
  , "/* Create a new buffer of the given size. */"
  , "Buffer newBuffer (const unsigned int size);"
  , ""
  , "/* Deallocate the buffer. */"
  , "void freeBuffer (Buffer buffer);"
  , ""
  , "/* Deallocate the buffer, but return its content as string. */"
  , "char* releaseBuffer (Buffer buffer);"
  , ""
  , "/* Clear contents of buffer. */"
  , "void resetBuffer (Buffer buffer);"
  , ""
  , "/* Append string at the end of the buffer. */"
  , "void bufferAppendString (Buffer buffer, const char *s);"
  , ""
  , "/* Append single character at the end of the buffer. */"
  , "void bufferAppendChar (Buffer buffer, const char c);"
  , ""
  , "/* Give read-only access to the buffer content. */"
  , "const char* bufferContent (Buffer buffer);"
  , ""
  , "#endif"
  ]

-- | A tiny buffer library for string buffers in the lexer.

bufferC :: String -> String
bufferC bufferH = unlines
  [ "/* A dynamically allocated character buffer that grows as it is appended. */"
  , ""
  , "#include <assert.h>  /* assert */"
  , "#include <stdlib.h>  /* free, malloc */"
  , "#include <stdio.h>   /* fprintf */"
  , "#include <string.h>  /* size_t, strncpy */"
  , "#include \"" ++ bufferH ++ "\""
  , ""
  , "/* Internal functions. */"
  , "/************************************************************************/"
  , ""
  , "/* Make sure the buffer can hold `n` more characters. */"
  , "static void bufferAllocateChars (Buffer buffer, const unsigned int n);"
  , ""
  , "/* Increase the buffer size to the new `buffer->size`. */"
  , "static void resizeBuffer(Buffer buffer);"
  , ""
  , "/* External interface. */"
  , "/************************************************************************/"
  , ""
  , "/* Create a new buffer of the given size. */"
  , ""
  , "Buffer newBuffer (const unsigned int size) {"
  , ""
  , "  /* The buffer cannot be of size 0. */"
  , "  assert (size >= 1);"
  , ""
  , "  /* Allocate and initialize a new Buffer structure. */"
  , "  Buffer buffer    = (Buffer) malloc(sizeof(struct buffer));"
  , "  buffer->size     = size;"
  , "  buffer->current  = 0;"
  , "  buffer->chars    = NULL;"
  , "  resizeBuffer(buffer);"
  , "  buffer->chars[0] = 0;"
  , "  return buffer;"
  , "}"
  , ""
  , "/* Deallocate the buffer and its content. */"
  , ""
  , "void freeBuffer (Buffer buffer) {"
  , "  free(buffer->chars);"
  , "  free(buffer);"
  , "}"
  , ""
  , "/* Deallocate the buffer, but return its content as string. */"
  , ""
  , "char* releaseBuffer (Buffer buffer) {"
  , "  char* content = (char*) realloc (buffer->chars, buffer->current + 1);"
  , "  free(buffer);"
  , "  return content;"
  , "}"
  , ""
  , "/* Clear contents of buffer. */"
  , ""
  , "void resetBuffer (Buffer buffer) {"
  , "  buffer->current = 0;"
  , "  buffer->chars[buffer->current] = 0;"
  , "}"
  , ""
  , "/* Append string at the end of the buffer. */"
  , ""
  , "void bufferAppendString (Buffer buffer, const char *s)"
  , "{"
  , "  /* Nothing to do if s is the empty string. */"
  , "  size_t len = strlen(s);"
  , "  if (len) {"
  , ""
  , "    /* Make sure the buffer can hold all of s. */"
  , "    bufferAllocateChars(buffer, len);"
  , ""
  , "    /* Append s at the end of the buffer, including terminating 0. */"
  , "    strncpy(buffer->chars + buffer->current, s, len + 1);"
  , "    buffer->current += len;"
  , "  }"
  , "}"
  , ""
  , "/* Append single character at the end of the buffer. */"
  , ""
  , "void bufferAppendChar (Buffer buffer, const char c)"
  , "{"
  , "  /* Make sure the buffer can hold one more character and append it. */"
  , "  bufferAllocateChars(buffer, 1);"
  , "  buffer->chars[buffer->current] = c;"
  , ""
  , "  /* Terminate with 0. */"
  , "  buffer->current++;"
  , "  buffer->chars[buffer->current] = 0;"
  , "}"
  , ""
  , "/* Give read-only access to the buffer content."
  , "   Does not survive the destruction of the buffer object. */"
  , ""
  , "const char* bufferContent (Buffer buffer) {"
  , "  return buffer->chars;"
  , "}"
  , ""
  , "/* Internal functions. */"
  , "/************************************************************************/"
  , ""
  , "/* Make sure the buffer can hold `n` more characters. */"
  , ""
  , "static void bufferAllocateChars (Buffer buffer, const unsigned int n) {"
  , "  /* 1 extra char for terminating 0. */"
  , "  unsigned int requiredSize = buffer->current + 1 + n;"
  , "  if (buffer->size < requiredSize)"
  , "  {"
  , "    do buffer->size *= 2; /* Double the buffer size */"
  , "      while (buffer->size < requiredSize);"
  , "    resizeBuffer(buffer);"
  , "  }"
  , "}"
  , ""
  , "/* Increase the buffer size to the new `size`. */"
  , ""
  , "static void resizeBuffer(Buffer buffer)"
  , "{"
  , "  /* The new size needs to be strictly greater than the currently"
  , "   * used part, otherwise writing to position buffer->current will"
  , "   * be out of bounds."
  , "   */"
  , "  assert(buffer->size > buffer->current);"
  , ""
  , "  /* Resize (or, the first time allocate) the buffer. */"
  , "  buffer->chars = (char*) realloc(buffer->chars, buffer->size);"
  , ""
  , "  /* Crash if out-of-memory. */"
  , "  if (! buffer->chars)"
  , "  {"
  , "    fprintf(stderr, \"Buffer.c: Error: Out of memory while attempting to grow buffer!\\n\");"
  , "    exit(1);  /* This seems to be the right exit code for out-of-memory. 137 is only when the OS kills us. */"
  , "  }"
  , "}"
  ]
