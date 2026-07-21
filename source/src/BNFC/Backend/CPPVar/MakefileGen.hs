{-# LANGUAGE QuasiQuotes #-}
{-|
  Module      : BNFC.Backend.CPPVar.MakefileGen
  Description : Makefile generator.

  Makefile generator.
-}

module BNFC.Backend.CPPVar.MakefileGen
  (
    -- * The entrypoint
    makeMakefile
  ) where

import Data.String.QQ (s)

import Text.PrettyPrint (Doc, text, ($+$), empty)
import qualified BNFC.Options as Options

import BNFC.Backend.CPPVar.CPPUtil

-- | Generates an example Makefile to compile the parser.
makeMakefile ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> Doc
makeMakefile opts =
  variables langname makefileName maybeOldBnfcOptions
  $++$ helpTextVariable langname makeBnfcTarget
  $++$ rules langname makeBnfcTarget
  where
    langname = Options.lang opts
    Just makefileName = Options.optMake opts
    makeBnfcTarget = Options.isDefault Options.outDir opts
    -- | We do not overwrite the Makefile.
    oldBnfcOptions = Options.printOptions opts { Options.optMake = Nothing }
    maybeOldBnfcOptions
      | makeBnfcTarget = Just oldBnfcOptions
      | otherwise      = Nothing

------------------------------------------------------------------------
-- * Utility.
------------------------------------------------------------------------

-- | Generates a table of available targets with descriptions.
-- Allows to specify the (minimal) width.
makeHelpTable ::
     Int                 -- ^ The minimal width.
  -> [(String, String)]  -- ^ Target names and descriptions.
  -> Doc
makeHelpTable minWidth rows =
  text (makeRow sTarget sDescription)
  $+$ text (replicate (leftColSize + 1) '-'
      ++ ('|' : replicate (rightColSize + 1) '-'))
  $+$ linesToText (map (uncurry makeRow) rows)
  where
    sTarget :: String
    sTarget = "TARGET"
    sDescription :: String
    sDescription = "DESCRIPTION"
    leftColSize = foldr max (length sTarget) $ map (length . fst) rows
    rightColSizeUnadjusted = foldr max (length sDescription)
      $ map (length . snd) rows
    width = max minWidth (leftColSize + rightColSizeUnadjusted + 3)
    rightColSize = width - leftColSize - 3
    makeRow target desc = concat
      [target, replicate (leftColSize - length target) ' ', " | " , desc]

-- | Generates a table of available targets with descriptions.
-- The table will be at least 80 characters wide.
makeHelpTable80 :: [(String, String)] -> Doc
makeHelpTable80 = makeHelpTable 80

------------------------------------------------------------------------
-- * Code generation.
------------------------------------------------------------------------

-- | Variable definitions.
variables ::
     String        -- ^ Language name.
  -> String        -- ^ Makefile name.
  -> Maybe String  -- ^ Nothing if no @bnfc@ target, else Just bnfc_options.
  -> Doc
variables langname makefileName maybeOldBnfcOptions = unlinesToText [s|
CXXFLAGS = -std=c++17 -Wall -Wextra
CXXFLAGS_BISON = $(CXXFLAGS) -Wno-unused-but-set-variable
BISONFLAGS =
|] $++$
    case maybeOldBnfcOptions of
      Nothing -> empty
      Just oldBnfcOptions -> linesToText
        [ "OLD_BNFCFLAGS = "
          ++ oldBnfcOptions
        , "BNFCFLAGS = $(OLD_BNFCFLAGS)"
        ]
  $++$ linesToText
  [ "OBJECTS = Absyn.o \\"
  , "\tClassicPrettyPrinter.o \\"
  , "\tContextFreePrettyPrinter.o \\"
  , "\tHaskellPrinter.o \\"
  , "\tPrinterCommon.o \\"
  , "\tSyntaxPrinter.o \\"
  , "\tTest.o \\"
  , "\t" ++ langname ++ ".lex.o \\"
  , "\t" ++ langname ++ ".tab.o"
  , ""
  , "ARCHIVES = lib" ++ langname ++ "Parser.a lib" ++ langname ++ "Printer.a"
  , ""
  , "BNFC_GENERATED = Absyn.cpp \\"
  , "\tAbsyn.hpp \\"
  , "\tClassicPrettyPrinter.cpp \\"
  , "\tClassicPrettyPrinter.hpp \\"
  , "\tContextFreePrettyPrinter.cpp \\"
  , "\tContextFreePrettyPrinter.hpp \\"
  , "\tHaskellPrinter.cpp \\"
  , "\tHaskellPrinter.hpp \\"
  , "\tLocations.hpp \\"
  , "\tPatternMatching.hpp \\"
  , "\tPrinterCommon.cpp \\"
  , "\tPrinterCommon.hpp \\"
  , "\tREADME.md \\"
  , "\tSyntaxPrinter.cpp \\"
  , "\tSyntaxPrinter.hpp \\"
  , "\tTest.cpp \\"
  , "\t" ++ langname ++ ".l \\"
  , "\t" ++ langname ++ ".ypp \\"
  , "\t" ++ makefileName
  ] $++$ unlinesToText [s|
TEST_DFLAGS =
ifneq ($(NO_PRETTY),)
TEST_DFLAGS += -DNO_PRETTY
endif
ifneq ($(NO_CPRETTY),)
TEST_DFLAGS += -DNO_CPRETTY
endif
ifneq ($(NO_TREE),)
TEST_DFLAGS += -DNO_TREE
endif
ifneq ($(NO_HASKELL),)
TEST_DFLAGS += -DNO_HASKELL
endif

LIBPRINTER_DEPS = PrinterCommon.o
ifeq ($(NO_PRETTY),)
LIBPRINTER_DEPS += ContextFreePrettyPrinter.o
endif
ifeq ($(NO_CPRETTY),)
LIBPRINTER_DEPS += ClassicPrettyPrinter.o
endif
ifeq ($(NO_TREE),)
LIBPRINTER_DEPS += SyntaxPrinter.o
endif
ifeq ($(NO_HASKELL),)
LIBPRINTER_DEPS += HaskellPrinter.o
endif

TEST_PRINTER_HEADERS =
ifeq ($(NO_PRETTY),)
TEST_PRINTER_HEADERS += ContextFreePrettyPrinter.hpp
endif
ifeq ($(NO_CPRETTY),)
TEST_PRINTER_HEADERS += ClassicPrettyPrinter.hpp
endif
ifeq ($(NO_TREE),)
TEST_PRINTER_HEADERS += SyntaxPrinter.hpp
endif
ifeq ($(NO_HASKELL),)
TEST_PRINTER_HEADERS += HaskellPrinter.hpp
endif
|]

-- | A help table for phony targets.
phonyHelp ::
     String  -- ^ The language name, given by 'BNFC.Options.lang'.
  -> Bool    -- ^ Do we make the @bnfc@ target?
  -> Doc
phonyHelp langname needBnfcTarget = makeHelpTable80 $ concat
  [ [ ("help",          "Print this message.")
    , ("all (default)", "Build the static libraries and the \"Test"
                        ++ langname ++ "\" program.")
    , ("bison",         "Invoke Bison.")
    ]
  , if needBnfcTarget
    then [("bnfc",      "Invoke BNFC again (see BNFCFLAGS).")]
    else []
  , [ ("mostlyclean",   "Delete all object/executable files.")
    , ("clean",   "Delete all files created by this Makefile and BNFC backups.")
    , ("distclean",     "Delete ALL files generated by BNFC or this Makefile.")
    ]
  ]

-- | A help table for file targets.
nonPhonyHelp :: String -> Doc
nonPhonyHelp langname = makeHelpTable80
  [ ("Test" ++ langname, "A program that performs parsing and prints the AST.")
  , ("lib" ++ langname ++ "Parser.a",
    "Implements the syntax tree, lexing, and parsing.")
  , ("lib" ++ langname ++ "Printer.a",
    "Implements pretty- and syntax-tree-printing.")
  , (langname ++ ".lex.cpp", "Contains the lexer implementation.")
  , ("Parser.hpp",           "Contain the syntax parser interface.")
  , (langname ++ ".tab.cpp", "Contain the syntax parser implementation.")
  ]

-- | Definition of the @HELPMESSAGE@ variable. Contains the help text.
helpTextVariable ::
     String  -- ^ Language name.
  -> Bool    -- ^ Do we make the @bnfc@ target?
  -> Doc
helpTextVariable langname needBnfcTarget = text "define HELPMESSAGE"
  $+$ text "Phony targets:" $++$ phonyHelp langname needBnfcTarget
  $++$ text "File targets:" $++$ nonPhonyHelp langname
  $++$ unlinesToText [s|
For any *.cpp file, a corresponding *.o file can be built.

Variables:

VARIABLE   | DESCRIPTION
-----------|--------------------------------------------------------------------
CXX        | The C++ compiler binary.
CXXFLAGS   | The flags to pass to the C++ compiler.
BISONFLAGS | The flags to pass to Bison.
LDFLAGS    | The flags to pass to the C++ linker.
|] $+$ (
    if needBnfcTarget
    then text [s|
BNFCFLAGS  | The flags to pass to BNFC when invoking it on the same grammar.
|]
    else empty
  ) $+$ unlinesToText [s|
NO_PRETTY  | Set to a nonempty string to not use the ContextFreePrettyPrinter.
NO_CPRETTY | Set to a nonempty string to not use the ClassicPrettyPrinter.
NO_TREE    | Set to a nonempty string to not use the SyntaxPrinter.
NO_HASKELL | Set to a nonempty string to not use the HaskellPrinter.

The --systest option is available in the Testgrammar program when
ClassicPrettyPrinter and HaskellPrinter are available.
endef
|]

rules ::
     String  -- ^ Language name.
  -> Bool    -- ^ Do we make the @bnfc@ target?
  -> Doc
rules langname needBnfcTarget = linesToText
  [ ".PHONY: all clean mostlyclean distclean default help bison" ++
      if needBnfcTarget
      then " bnfc"
      else ""
  , ""
  , "default: all"
  , ""
  , "help:"
  , "\t$(file >/dev/stdout,$(HELPMESSAGE))"
  , "\t@:"
  , ""
  , "all: $(ARCHIVES) Test" ++ langname
  ] $++$ (
    if needBnfcTarget
    then linesToText
      [ "bnfc:"
      , "\tbnfc $(BNFCFLAGS)"
      ]
    else empty
  ) $+$ linesToText
  [ "define NEWLINE"
  , ""
  , ""
  , ""
  , "endef"
  , "MOSTLYCLEAN_FILES := $(OBJECTS) $(ARCHIVES) Test" ++ langname
  , "BAKFILES := $(foreach f,$(BNFC_GENERATED),$(f).bak)"
  , "CLEAN_FILES := " ++ langname ++ ".lex.cpp " ++ langname ++ ".tab.cpp "
    ++ "Parser.hpp " ++ langname ++ ".loc.hpp $(BAKFILES)"
  , ""
  , "mostlyclean:"
  , "\t$(foreach f,$(wildcard $(MOSTLYCLEAN_FILES)),rm $(f);$(NEWLINE))"
  , ""
  , "clean: mostlyclean"
  , "\t$(foreach f,$(wildcard $(CLEAN_FILES)),rm $(f);$(NEWLINE))"
  , ""
  , "distclean: clean"
  , "\t$(foreach f,$(wildcard $(BNFC_GENERATED)),rm $(f);$(NEWLINE))"
  , ""
  , "Locations.hpp: " ++ langname ++ ".loc.hpp"
  , "\ttouch $@"
  , ""
  , langname ++ ".lex.cpp: " ++ langname ++ ".l"
  , "\tflex " ++ langname ++ ".l"
  , ""
  , "INVOKE_BISON = bison $(BISONFLAGS) " ++ langname ++ ".ypp"
  , "bison:"
  , "\t$(INVOKE_BISON)"
  , ""
  , "Parser.hpp " ++ langname ++ ".tab.cpp " ++ langname ++ ".loc.hpp &: "
    ++ langname ++ ".ypp"
  , "\t$(INVOKE_BISON)"
  , ""
  , "CXX_COMPILE = $(CXX) $(CXXFLAGS) -c -o $@ $<"
  , "Absyn.o: Absyn.cpp Absyn.hpp Locations.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , langname ++ ".tab.o: " ++ langname ++ ".tab.cpp Parser.hpp \\"
  , "\tAbsyn.hpp Locations.hpp PatternMatching.hpp"
  , "\t$(CXX) $(CXXFLAGS_BISON) -c -o $@ $<"
  , ""
  , langname ++ ".lex.o: " ++ langname ++ ".lex.cpp Parser.hpp \\"
  , "\tLocations.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "PrinterCommon.o: PrinterCommon.cpp PrinterCommon.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "HaskellPrinter.o: HaskellPrinter.cpp HaskellPrinter.hpp \\"
  , "\tAbsyn.hpp Locations.hpp PrinterCommon.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "ClassicPrettyPrinter.o: ClassicPrettyPrinter.cpp \\"
  , "\tClassicPrettyPrinter.hpp Absyn.hpp PrinterCommon.hpp \\"
  , "\tLocations.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "ContextFreePrettyPrinter.o: ContextFreePrettyPrinter.cpp \\"
  , "\tContextFreePrettyPrinter.hpp Absyn.hpp PrinterCommon.hpp \\"
  , "\tLocations.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "SyntaxPrinter.o: SyntaxPrinter.cpp SyntaxPrinter.hpp \\"
  , "\tAbsyn.hpp PrinterCommon.hpp Locations.hpp"
  , "\t$(CXX_COMPILE)"
  , ""
  , "AR_COMPILE = $(AR) $(ARFLAGS) $@ $^"
  , "lib" ++ langname ++ "Parser.a: Absyn.o " ++ langname ++ ".lex.o "
    ++ langname ++ ".tab.o"
  , "\t$(AR_COMPILE)"
  , ""
  , "lib" ++ langname ++ "Printer.a: $(LIBPRINTER_DEPS)"
  , "\t$(AR_COMPILE)"
  , ""
  , "Test.o: Test.cpp Absyn.hpp Parser.hpp Locations.hpp \\"
  , "\tPatternMatching.hpp $(TEST_PRINTER_HEADERS)"
  , "\t$(CXX) $(CXXFLAGS) $(TEST_DFLAGS) -c -o $@ $<"
  , ""
  , "Test" ++ langname ++ ": Test.o lib" ++ langname ++ "Parser.a lib"
    ++ langname ++ "Printer.a"
  , "\t$(CXX) $(LDFLAGS) $^ -o Test" ++ langname
  ]
