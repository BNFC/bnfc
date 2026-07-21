{-|
  Module      : BNFC.Backend.CPPVar
  Description : C++17 backend using FLex, Bison, and the standard template
                library (especially std::variant).

  C++17 backend using FLex, Bison, and the standard template library
  (especially std::variant).
-}

module BNFC.Backend.CPPVar
  (
    -- * The entrypoint
    makeCppVar
  ) where

import BNFC.CF
import BNFC.Options
import BNFC.Backend.Base

import qualified BNFC.Backend.CPPVar.CPPUtil as CPPUtil
import qualified BNFC.Backend.CPPVar.AbsynGen as AbsynGen
import qualified BNFC.Backend.CPPVar.FlexGen as FlexGen
import qualified BNFC.Backend.CPPVar.BisonGen as BisonGen
import qualified BNFC.Backend.CPPVar.PatternMatchingGen as PatternMatchingGen
import qualified BNFC.Backend.CPPVar.PrinterUtils as PrinterUtils
import qualified BNFC.Backend.CPPVar.SyntaxPrinterGen as SyntaxPrinterGen
import qualified BNFC.Backend.CPPVar.CFPrettyPrinterGen as CFPrettyPrinterGen
import qualified BNFC.Backend.CPPVar.ClassicPrettyPrinterGen
  as ClassicPrettyPrinterGen
import qualified BNFC.Backend.CPPVar.HaskellPrinterGen as HaskellPrinterGen
import qualified BNFC.Backend.CPPVar.TestGen as TestGen
import qualified BNFC.Backend.CPPVar.PrinterCommonGen as PrinterCommonGen
import qualified BNFC.Backend.CPPVar.MakefileGen as MakefileGen
import qualified BNFC.Backend.CPPVar.ReadmeGen as ReadmeGen

{-| Generates the following files (@language@ is the basename of the LBNF
grammar description file, as given by 'BNFC.Options.lang'):

+----------------------------+-------------------------------------------+
|            FILE            |                DESCRIPTION                |
+============================+===========================================+
| @Absyn.hpp@                |                                           |
+----------------------------+ abstract syntax node declarations         |
| @Absyn.cpp@                |                                           |
+----------------------------+-------------------------------------------+
| @PatternMatching.hpp@      | the pipe operator overload                |
+----------------------------+-------------------------------------------+
| @PrinterCommon.hpp@        |                                           |
+----------------------------+ common string visualization utilities     |
| @PrinterCommon.cpp@        |                                           |
+----------------------------+-------------------------------------------+
| @ContextFreePretty@        |                                           |
| @Printer.hpp@              |                                           |
+----------------------------+ pretty-printing (whole nodes at once)     |
| @ContextFreePretty@        |                                           |
| @Printer.cpp@              |                                           |
+----------------------------+-------------------------------------------+
| @ClassicPrettyPrinter.hpp@ |                                           |
+----------------------------+ pretty-printing (token-wise)              |
| @ClassicPrettyPrinter.cpp@ |                                           |
+----------------------------+-------------------------------------------+
| @HaskellPrinter.hpp@       | visualization of the abstract syntax tree |
+----------------------------+ (as a Haskell expression)                 |
| @HaskellPrinter.cpp@       |                                           |
+----------------------------+-------------------------------------------+
| @SyntaxPrinter.hpp@        | visualization of the abstract syntax tree |
+----------------------------+ (as an ASCII tree)                        |
| @SyntaxPrinter.cpp@        |                                           |
+----------------------------+-------------------------------------------+
| @language.l@               | the FLex lexer definition                 |
+----------------------------+-------------------------------------------+
| @language.ypp@             | the Bison parser definition               |
+----------------------------+-------------------------------------------+
| @Locations.hpp@            | provides location tracking structs        |
+----------------------------+-------------------------------------------+
| @Makefile@                 | recipes reference __(if @-m@ given)__     |
+----------------------------+-------------------------------------------+
| @Test.cpp@                 | an example parser, used for testing       |
+----------------------------+-------------------------------------------+
| @README.md@                | a short tutorial                          |
+----------------------------+-------------------------------------------+
-}

makeCppVar ::
     SharedOptions  -- ^ BNFC invokation options.
  -> CF             -- ^ The grammar description.
  -> MkFiles ()
makeCppVar opts cf@CFG
  { cfgPragmas        = cfPragmas
  , cfgLiterals       = cfLiterals
  , cfgSymbols        = cfSymbols
  , cfgKeywords       = cfKeywords
  , cfgRules          = cfRules
  } = do
  let
    groupedRules       = CPPUtil.groupRules cfRules
    mergedGroupedRules = CPPUtil.mergeCoercCats groupedRules
    entrypts           = CPPUtil.extractEntrypoints
      (defaultBisonEntrypoints opts) cfPragmas groupedRules cfRules
    cfTerminals        = cfSymbols ++ cfKeywords
    isPosToken         = CPPUtil.isPositionalToken cfPragmas
    AbsynGen.GeneratedAbsyn
      { absynCode = CPPUtil.CPPHeaderSourcePair
        { cppHeaderText = absynHpp
        , cppSourceText = absynCpp
        }
      , absynListItemStorage = listItemStorage
      } = AbsynGen.makeAbsyn opts cfLiterals cfPragmas isPosToken
        entrypts mergedGroupedRules
    FlexGen.CompiledLexer
      { compiledLexer_flexGrammar        = flexFile
      , compiledLexer_implicitTokenNames = implicitTokenNames
      } = FlexGen.makeFlex opts cfLiterals cfTerminals cfPragmas
    bisonFile = BisonGen.makeBison opts implicitTokenNames isPosToken cf
      groupedRules listItemStorage entrypts
    printables = PrinterUtils.getPrintableSymbols
      cfLiterals cfPragmas mergedGroupedRules
    CPPUtil.CPPHeaderSourcePair
      { cppHeaderText = syntaxHpp
      , cppSourceText = syntaxCpp
      } = SyntaxPrinterGen.makeSyntaxPrinter
        opts isPosToken printables listItemStorage
    CPPUtil.CPPHeaderSourcePair
      { cppHeaderText = prettyHpp
      , cppSourceText = prettyCpp
      } = CFPrettyPrinterGen.makePrettyPrinter opts printables listItemStorage
    CPPUtil.CPPHeaderSourcePair
      { cppHeaderText = haskellHpp
      , cppSourceText = haskellCpp
      } = HaskellPrinterGen.makeHaskellPrinter
        opts isPosToken printables listItemStorage
    CPPUtil.CPPHeaderSourcePair
      { cppHeaderText = cprettyHpp
      , cppSourceText = cprettyCpp
      } = ClassicPrettyPrinterGen.makeClassicPrettyPrinter
        opts printables listItemStorage
  mkfile AbsynGen.absynHppFilename comment absynHpp
  mkfile AbsynGen.absynCppFilename comment absynCpp
  mkfile (FlexGen.flexFilename opts) comment flexFile
  mkfile (BisonGen.bisonFilename opts) comment bisonFile
  mkfile BisonGen.locationHeaderFilename comment
    $ BisonGen.makeLocationHeader opts
  mkfile PatternMatchingGen.patternMatchingFilename comment
    PatternMatchingGen.patternMatchingHpp
  mkfile PrinterCommonGen.printerCommonHppFilename comment
    $ PrinterCommonGen.makePrinterCommonHpp opts
  mkfile PrinterCommonGen.printerCommonCppFilename comment
    $ PrinterCommonGen.makePrinterCommonCpp opts
  mkfile SyntaxPrinterGen.syntaxPrinterHppFilename comment syntaxHpp
  mkfile SyntaxPrinterGen.syntaxPrinterCppFilename comment syntaxCpp
  mkfile CFPrettyPrinterGen.prettyPrinterHppFilename comment prettyHpp
  mkfile CFPrettyPrinterGen.prettyPrinterCppFilename comment prettyCpp
  mkfile HaskellPrinterGen.haskellPrinterHppFilename comment haskellHpp
  mkfile HaskellPrinterGen.haskellPrinterCppFilename comment haskellCpp
  mkfile ClassicPrettyPrinterGen.classicPrettyPrinterHppFilename comment
    cprettyHpp
  mkfile ClassicPrettyPrinterGen.classicPrettyPrinterCppFilename comment
    cprettyCpp
  mkfile TestGen.testFilename comment (TestGen.makeTest opts)
  case optMake opts of
    Nothing           -> return ()
    Just makefileName -> mkfile makefileName ("# " ++)
      $ MakefileGen.makeMakefile opts
  mkfile ReadmeGen.readmeFilename mdcomment (ReadmeGen.makeReadme)

-- | C++ comment wrapper.
comment :: String -> String
comment = ("/* " ++) . (++ " */")

-- | Markdown comment wrapper.
mdcomment :: String -> String
mdcomment = ("<!-- " ++ ) . ( ++ " -->")
