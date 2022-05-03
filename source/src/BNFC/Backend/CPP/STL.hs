{-
    BNF Converter: C++ Main file
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer
    Copyright (C) 2020  Andreas Abel

    Modified from CPPTop to BNFC.Backend.CPP.STL 2006 by Aarne Ranta.

-}

module BNFC.Backend.CPP.STL (makeCppStl,) where

import Data.Foldable (toList)
import Data.List       ( nub )
import qualified Data.Map as Map
import Data.Maybe()

import BNFC.Utils
import BNFC.CF
import BNFC.Options
import BNFC.PrettyPrint
import BNFC.Backend.Common.OOAbstract
import BNFC.Backend.Base
import BNFC.Backend.C            ( bufferH, bufferC, comment, testfileHeader )
import BNFC.Backend.C.CFtoBisonC ( cf2Bison, unionBuiltinTokens, positionCats, varName )
import BNFC.Backend.C.CFtoFlexC  ( cf2flex, ParserMode(..), beyondAnsi, parserPackage, parserName, stlParser )
import BNFC.Backend.CPP.Common   ( commentWithEmacsModeHint, wrapSharedPtr )
import BNFC.Backend.CPP.Makefile
import BNFC.Backend.CPP.STL.CFtoSTLAbs
import BNFC.Backend.CPP.STL.CFtoCVisitSkelSTL
import BNFC.Backend.CPP.PrettyPrinter
import BNFC.Backend.CPP.STL.STLUtils
import qualified BNFC.Backend.Common.Makefile as Makefile

makeCppStl :: SharedOptions -> CF -> MkFiles ()
makeCppStl opts cf = do
  let (hfile, cfile) = cf2CPPAbs (linenumbers opts) cppStdMode (inPackage opts) name cf
  mkCppFile ("Absyn" ++ hExt) hfile
  mkCppFile ("Absyn" ++ cppExt) cfile
  mkCppFile ("Buffer" ++ hExt) bufferH
  mkCppFile ("Buffer" ++ cppExt) $ bufferC ("Buffer" ++ hExt)
  -- Generate xxx.ll file
  let (flex, env) = cf2flex parserMode cf
  mkCppFileWithHint (name ++ lexerExt) flex
  -- Generate xxx.yy file
  mkCppFileWithHint (name ++ parserExt) $ cf2Bison (linenumbers opts) parserMode cf env
  mkCppFile ("Parser" ++ hExt) $
    mkHeaderFile parserMode hExt (inPackage opts) cf (allParserCats cf) (toList $ allEntryPoints cf) (Map.elems env)
  mkCppFile ("ParserError" ++ hExt) $ printParseErrHeader (inPackage opts)
  let (skelH, skelC) = cf2CVisitSkel opts True (inPackage opts) cf
  mkCppFile ("Skeleton" ++ hExt) skelH
  mkCppFile ("Skeleton" ++ cppExt) skelC
  let (prinH, prinC) = cf2CPPPrinter cppStdMode True (inPackage opts) cf hExt
  mkCppFile ("Printer" ++ hExt) prinH
  mkCppFile ("Printer" ++ cppExt) prinC
  mkCppFile ("Test" ++ cppExt) (cpptest parserMode (inPackage opts) cf hExt)

  case (ansi opts) of
    BeyondAnsi -> do
      mkCppFile ("Driver" ++ cppExt) $ driverC parserMode cf ("Driver" ++ hExt)
      mkCppFile ("Driver" ++ hExt) $ driverH parserMode cf cats
      mkCppFile ("Scanner" ++ hExt) $ scannerH parserMode;
    _ ->
      return();
  Makefile.mkMakefile opts $ makefile prefix name opts
  where
    name :: String
    name = lang opts
    -- The prefix is a string used by flex and bison
    -- that is prepended to generated function names.
    -- It should be a valid C identifier.
    prefix :: String
    prefix = snakeCase_ name ++ "_"
    parserMode :: ParserMode
    parserMode = CppParser (inPackage opts) prefix (ansi opts)
    mkCppFile         x = mkfile x comment
    mkCppFileWithHint x = mkfile x commentWithEmacsModeHint
    -- Switch C++ generator module
    cppStdMode :: CppStdMode
    cppStdMode = if Ansi == ansi opts then CppStdAnsi (ansi opts) else CppStdBeyondAnsi (ansi opts)
    lexerExt = if Ansi == ansi opts then ".l" else ".ll"
    parserExt = if Ansi == ansi opts then ".y" else ".yy"
    cppExt = if Ansi == ansi opts then ".c" else ".cc"
    hExt = if Ansi == ansi opts then ".h" else ".hh"
    posCats
      | stlParser parserMode = map TokenCat $ positionCats cf
      | otherwise      = []
    cats = posCats ++ allParserCatsNorm cf


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

cpptest :: ParserMode -> Maybe String -> CF -> String -> String
cpptest mode inPackage cf hExt = unlines $ concat
  [ testfileHeader
  , [ ""
    , "#include <cstdio>"
    , "#include <string>"
    , "#include <iostream>"
    , if beyondAnsi mode then
        unlines [
        "#include <memory>"
        , "#include <fstream>"
        , "#include \"Driver" ++hExt++ "\""
        ]
      else
        "#include \"Parser" ++hExt++ "\""
    , "#include \"Printer" ++hExt++ "\""
    , "#include \"Absyn" ++hExt++ "\""
    , "#include \"ParserError" ++hExt++ "\""
    , ""
    , "void usage() {"
    , "    printf(\"usage: Call with one of the following argument combinations:\\n\");"
    , "    printf(\"\\t--help\\t\\tDisplay this help message.\\n\");"
    , "    printf(\"\\t(no arguments)\\tParse stdin verbosely.\\n\");"
    , "    printf(\"\\t(files)\\t\\tParse content of files verbosely.\\n\");"
    , "    printf(\"\\t-s (files)\\tSilent mode. Parse content of files silently.\\n\");"
    , "}"
    , ""
    , "int main(int argc, char ** argv)"
    , if beyondAnsi mode then
        unlines [
        "{"
        , "    int quiet = 0;"
        , "    char *filename = NULL;"
        , ""
        , "    if (argc > 1) {"
        , "        if (strcmp(argv[1], \"-s\") == 0) {"
        , "            quiet = 1;"
        , "            if (argc > 2) {"
        , "                filename = argv[2];"
        , "            }"
        , "        } else {"
        , "            filename = argv[1];"
        , "        }"
        , "    }"
        , ""
        , "    /* The default entry point is used. For other options see Parser.H */"
        , "    " ++ (wrapSharedPtr $ scope ++ dat) ++ " parse_tree = nullptr;"
        , "    try { "
        , ""
        , "        auto driver = std::make_unique<" ++nsScope driverNS++camelCaseName++ "Driver>();"
        , "        if (filename) {"
        , "            std::ifstream input(filename);"
        , "            if ( ! input.good() ) {"
        , "                usage();"
        , "                exit(1);"
        , "            }"
        , "            parse_tree = driver->p" ++ def ++ "(input);"
        , "        } else {"
        , "            parse_tree = driver->p" ++ def ++ "(std::cin);"
        , "        }"
        ]
      else
        unlines [
        "{"
        , "    FILE *input;"
        , "    int quiet = 0;"
        , "    char *filename = NULL;"
        , ""
        , "    if (argc > 1) {"
        , "        if (strcmp(argv[1], \"-s\") == 0) {"
        , "            quiet = 1;"
        , "            if (argc > 2) {"
        , "                filename = argv[2];"
        , "            } else {"
        , "                input = stdin;"
        , "            }"
        , "        } else {"
        , "            filename = argv[1];"
        , "        }"
        , "    }"
        , ""
        , "    if (filename) {"
        , "        input = fopen(filename, \"r\");"
        , "        if (!input) {"
        , "            usage();"
        , "            exit(1);"
        , "        }"
        , "    } else input = stdin;"
        , ""
        , "    /* The default entry point is used. For other options see Parser.H */"
        , "    " ++ scope ++ dat ++ " *parse_tree = NULL;"
        , "    try { "
        ,"        parse_tree = p" ++ def ++ "(input);"
        ]
    , "    } catch( " ++ scope ++ "parse_error &e) {"
    , "        std::cerr << \"Parse error on line \" << e.getLine() << \"\\n\"; "
    , "    }"
    , ""
    , "    if (parse_tree)"
    , "    {"
    , "        printf(\"\\nParse Successful!\\n\");"
    , if beyondAnsi mode then
        unlines [
          "        if (!quiet) {"
        , "            printf(\"\\n[Abstract Syntax]\\n\");"
        , "            auto s = std::make_unique<" ++nsScope ns++"ShowAbsyn>(" ++nsScope ns++"ShowAbsyn());"
        , "            printf(\"%s\\n\\n\", s->show(parse_tree.get()));"
        , "            printf(\"[Linearized Tree]\\n\");"
        , "            auto p = std::make_unique<" ++nsScope ns++"PrintAbsyn>(" ++nsScope ns++"PrintAbsyn());"
        , "            printf(\"%s\\n\\n\", p->print(parse_tree.get()));"
        , "      }"
        ]
      else
        unlines [
          "        if (!quiet) {"
        , "            printf(\"\\n[Abstract Syntax]\\n\");"
        , "            " ++nsScope ns++"ShowAbsyn *s = new " ++nsScope ns++"ShowAbsyn();"
        , "            printf(\"%s\\n\\n\", s->show(parse_tree));"
        , "            printf(\"[Linearized Tree]\\n\");"
        , "            " ++nsScope ns++"PrintAbsyn *p = new " ++nsScope ns++"PrintAbsyn();"
        , "            printf(\"%s\\n\\n\", p->print(parse_tree));"
        , "      }"
        , "      delete(parse_tree);"
        ]
    , "      return 0;"
    , "    }"
    , "    return 1;"
    , "}"
    ]
  ]
  where
   cat = firstEntry cf
   dat = identCat $ normCat cat
   def = identCat cat
   scope = nsScope inPackage
   name = parserName mode
   camelCaseName = camelCase_ name
   ns = inPackage
   driverNS = inPackage

mkHeaderFile :: ParserMode -> String -> Maybe String -> CF -> [Cat] -> [Cat] -> [String] -> String
mkHeaderFile mode hExt inPackage _cf _cats eps _env = unlines $ concat
  [ [ "#ifndef " ++ hdef
    , "#define " ++ hdef
    , ""
    , "#include <vector>"
    , "#include <string>"
    , "#include <cstdio>"
    , "#include \"Absyn" ++ hExt ++ "\""
    , if beyondAnsi mode then
        "#include \"Bison" ++ hExt ++ "\""
      else
        ""
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
  mkFuncs s = if beyondAnsi mode then
                [ identCat (normCat s) ++ "*" +++ "p" ++ identCat s ++ "(std::istream &stream);" ]
              else
                [ identCat (normCat s) ++ "*" +++ "p" ++ identCat s ++ "(FILE *inp);"
                , identCat (normCat s) ++ "*" +++ "p" ++ identCat s ++ "(const char *str);"
                ]



-- | C++ lexer/parser driver

driverH :: ParserMode -> CF -> [Cat] -> String
driverH mode cf cats = unlines
  [ "#ifndef __DRIVER_H__"
  , "#define __DRIVER_H__ 1"

  , "#include <string>"
  , "#include <cstddef>"
  , "#include <istream>"
  , "#include <memory>"
  , ""
  , "#include \"Scanner.hh\""
  , "#include \"Parser.hh\""
  , ""
  , nsStart ns
  , ""
  , "class  " ++camelCaseName++ "Driver{"
  , "public:"
  , "    " ++camelCaseName++ "Driver() = default;"
  , "    virtual ~ " ++camelCaseName++ "Driver();"
  , ""
  , "    /**"
  , "     * parser parsed values defined by bnfc"
  , "     */"
  -- bnfc builtin tokens
  , unlines [ prettyShow ("    " ++ tok) | tok <- unionBuiltinTokens ]
  -- user defined tokens
  , unlines [ prettyShow ("    std::shared_ptr<" ++ identCat tok ++ ">"+++ varName tok ++";") | tok <- normCats ]
  , ""
  , unlines [ mkStreamEntry ep | ep <- entryPoints ]
  , ""
  , "    /**"
  , "     * parse - parse from a file"
  , "     * @param filename - valid string with input file"
  , "     */"
  , "    void parse(const char *filename);"
  , "    /**"
  , "     * parse - parse from a c++ input stream"
  , "     * @param is - std::istream&, valid input stream"
  , "     */"
  , "    void parse(std::istream &iss);"
  , "    /** Error handling with associated line number. This can be modified to output the error. */"
  , "    void error(const " ++nsScope parserNs++camelCaseName++ "Parser::location_type& l, const std::string& m);"
  , ""
  , "    std::ostream& print(std::ostream &stream);"
  , ""
  , "    // debug flags"
  , "    bool trace_scanning = false;"
  , "    bool trace_parsing = false;"
  , ""
  , "    std::unique_ptr<" ++camelCaseName++ "Scanner> scanner = nullptr;"
  , "    std::unique_ptr<" ++ nsScope parserNs ++camelCaseName++ "Parser>  parser  = nullptr;"
  , ""
  , "private:"
  , "    void parse_helper( std::istream &stream );"
  , ""
  , "};"
  , ""
  , nsEnd ns
  , "#endif /* END __DRIVER_H__ */"
  ]
  where
    name = parserName mode
    camelCaseName = camelCase_ name
    ns = parserPackage mode -- bnfc -p "package"
    parserNs = case ns of
      Just ns -> Just ns;   -- Using above namespace
      Nothing -> Just name; -- Using namespace generated by bison (see Makefile)
    normCats = nub (map normCat cats)
    entryPoints = toList (allEntryPoints cf)
    mkStreamEntry s =
      "    " ++ (wrapSharedPtr $ identCat (normCat s)) +++ "p" ++ identCat s ++ "(std::istream &stream);"


-- | C++ lexer/parser driver

driverC :: ParserMode -> CF -> String -> String
driverC mode cf _ = unlines
  [ "#include <cctype>"
  , "#include <fstream>"
  , "#include <cassert>"
  , ""
  , "#include \"Driver.hh\""
  , nsStart ns
  , ""
  , "" ++camelCaseName++ "Driver::~" ++camelCaseName++ "Driver()"
  , "{"
  , "}"
  , " "
  , "void "
  , camelCaseName++ "Driver::parse( const char * const filename )"
  , "{"
  , "    /**"
  , "     * Remember, if you want to have checks in release mode"
  , "     * then this needs to be an if statement "
  , "     */"
  , "    assert( filename != nullptr );"
  , "    std::ifstream in_file( filename );"
  , "    if( ! in_file.good() )"
  , "    {"
  , "        exit( EXIT_FAILURE );"
  , "    }"
  , "    parse_helper( in_file );"
  , "    return;"
  , "}"
  , " "
  , "void"
  , camelCaseName++ "Driver::parse( std::istream &stream )"
  , "{"
  , "    if( ! stream.good()  && stream.eof() ) {"
  , "        return;"
  , "    }"
  , "    parse_helper( stream ); "
  , "    return;"
  , "}"
  , " "
  , "void"
  , camelCaseName++ "Driver::error( const " ++nsScope parserNs++camelCaseName++ "Parser::location_type& l, const std::string& m )"
  , "{"
  , "    std::cerr << \"error: \""
  , "              << scanner->loc->begin.line << \",\" << scanner->loc->begin.column"
  , "              << \": \""
  , "              << m"
  , "              << \" at \" << std::string(scanner->YYText())"
  , "              << std::endl;"
  , "}"
  , ""
  , "void "
  , camelCaseName++ "Driver::parse_helper( std::istream &stream )"
  , "{"
  , ""
  , "    scanner.reset();"
  , "    try {"
  , "        scanner = std::make_unique<" ++camelCaseName++ "Scanner>( &stream );"
  , "        scanner->set_debug(trace_scanning);"
  , "    } catch( std::bad_alloc &ba ) {"
  , "        std::cerr << \"Failed to allocate scanner: (\""
  , "                  << ba.what() "
  , "                  << \"), exiting!!\\n\";"
  , "        exit( EXIT_FAILURE );"
  , "    }"
  , ""
  , "    parser.reset(); "
  , "    try {"
  , "        parser = std::make_unique<" ++nsScope parserNs++camelCaseName++ "Parser>((*scanner), (*this));"
  , "    } catch( std::bad_alloc &ba ) {"
  , "        std::cerr << \"Failed to allocate parser: (\""
  , "                  << ba.what() "
  , "                  << \"), exiting!!\\n\";"
  , "        exit( EXIT_FAILURE );"
  , "    }"
  , "    const int accept( 0 );"
  , ""
  , "    parser->set_debug_level (trace_parsing);"
  , "    if( parser->parse() != accept ) {"
  , "        exit( EXIT_FAILURE );"
  , "    }"
  , "    return;"
  , "}"
  , ""
  , unlines [ mkStreamEntry ep | ep <- entryPoints ]
  , nsEnd ns
  ]
  where
    name = parserName mode
    camelCaseName = camelCase_ name
    ns = parserPackage mode -- bnfc -p "package"
    parserNs = case ns of
      Just _ -> ns;         -- Using above namespace, so not necessary parser name namespace
      Nothing -> Just name; -- Using namespace generated by bison (see Makefile)
    entryPoints = toList (allEntryPoints cf)
    reversibleCats = cfgReversibleCats cf

    mkStreamEntry s =
      unlines [
      (wrapSharedPtr $ identCat (normCat s))
      , camelCaseName++ "Driver::p" ++ identCat s ++ "(std::istream &stream)"
      , "{"
      , "    parse_helper( stream );"
      , if isList s && not (s `elem` reversibleCats) then
          "    this->" ++ varName s++ "->reverse();"
        else
          ""
      , "    return this->" ++ varName s++ ";"
      , "}"
      ]

-- | C++ lexer def (scanner.hh)

scannerH :: ParserMode -> String
scannerH mode = unlines
  [ "#ifndef __SCANNER_H__"
  , "#define __SCANNER_H__ 1"
  , ""
  , "// Flex expects the signature of yylex to be defined in the macro YY_DECL, and"
  , "// the C++ parser expects it to be declared."
  , "#ifndef YY_DECL"
  , "#define YY_DECL \\"
  , "    int         \\"
  , "    " ++ nsScope ns ++camelCaseName++ "Scanner::lex(                        \\"
  , "    " ++ nsScope parserNs ++camelCaseName++ "Parser::semantic_type* const yylval, \\"
  , "    " ++ nsScope parserNs ++camelCaseName++ "Parser::location_type* yylloc        \\"
  , "    )"
  , "#endif"
  , ""
  -- Inherit from yyFlexLexer, create a subclass with naming "XXXScanner"
  -- https://stackoverflow.com/a/40665154/2565527
  , "#if !defined(yyFlexLexerOnce)"
  , "#   include \"FlexLexer.h\""
  , "#endif"
  , ""
  , "#include \"Bison.hh\""
  , "#include \"location.hh\""
  , ""
  , nsStart ns
  , ""
  , "class " ++camelCaseName++ "Scanner : public yyFlexLexer {"
  , "public:"
  , ""
  , "    " ++camelCaseName++ "Scanner(std::istream *in);"
  , "    virtual ~" ++camelCaseName++ "Scanner();"
  , ""
  , "    virtual"
  , "    int lex( " ++ nsScope parserNs ++camelCaseName++ "Parser::semantic_type * const lval,"
  , "             " ++ nsScope parserNs ++camelCaseName++ "Parser::location_type *location );"
  , "    // YY_DECL defined in .ll file. Method body created by flex in Lexer.cc"
  , ""
  , "    /* yyval ptr */"
  , "    " ++ nsScope parserNs ++camelCaseName++ "Parser::semantic_type *yylval = nullptr;"
  , "    /* location ptr */"
  , "    " ++ nsScope parserNs ++camelCaseName++ "Parser::location_type *loc    = nullptr;"
  , "};"
  , ""
  , nsEnd ns
  ,  ""
  , "#endif /* END __SCANNER_H__ */"
  ]
  where
    name = parserName mode
    camelCaseName = camelCase_ name
    ns = parserPackage mode  -- bnfc -p "package"
    parserNs = case ns of
      Just _ -> Nothing;     -- Using above namespace, so not necessary parser name namespace
      Nothing -> Just name;  -- Using namespace generated by bison (see Makefile)
