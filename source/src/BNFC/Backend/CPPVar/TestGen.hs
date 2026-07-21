{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.TestGen
  Description : Generates an example parser for testing.

  Generates an example parser for testing.
-}

module BNFC.Backend.CPPVar.TestGen
  (
    -- * The entrypoint
    makeTest

    -- * File naming
  , testFilename
  ) where

import Data.String.QQ
import Text.PrettyPrint

import qualified BNFC.Options as Options
import BNFC.Backend.CPPVar.CPPUtil

-- | The name of the source file.
testFilename :: String
testFilename = "Test.cpp"

-- | Generates the example parser code.
makeTest :: Options.SharedOptions -> Doc
makeTest opts = let
    ns = nsutils_prefix $ newNamespaceUtilsFromOptions opts
  in unlinesToText [s|
#include <cstring>
#include <vector>

#undef YES_PRETTY
#undef YES_CPRETTY
#undef YES_TREE
#undef YES_HASKELL
#undef YES_SYSTEST

#ifndef NO_PRETTY
#define YES_PRETTY
#endif
#ifndef NO_CPRETTY
#define YES_CPRETTY
#endif
#ifndef NO_TREE
#define YES_TREE
#endif
#ifndef NO_HASKELL
#define YES_HASKELL
#endif
#if !defined(NO_HASKELL) && !defined(NO_CPRETTY)
#define YES_SYSTEST
#endif

#ifdef YES_HASKELL
#include "HaskellPrinter.hpp"
#endif
#ifdef YES_CPRETTY
#include "ClassicPrettyPrinter.hpp"
#endif
#ifdef YES_PRETTY
#include "ContextFreePrettyPrinter.hpp"
#endif
#ifdef YES_TREE
#include "SyntaxPrinter.hpp"
#endif
#include "Parser.hpp"
#include "PatternMatching.hpp"
using namespace std;

bool help = false;
#ifdef YES_PRETTY
bool pretty = false;
#endif
#ifdef YES_CPRETTY
bool cpretty = false;
#endif
#ifdef YES_TREE
bool tree = false;
#endif
#ifdef YES_HASKELL
bool haskell = false;
#endif
#ifdef YES_SYSTEST
bool systest = false;
#endif

int main(int argc, char** argv) {
    if (!argc) {
        cerr << "0 arguments provided" << endl;
        return 1;
    }
    bool onlyFiles = false;
    vector<const char*> files;
    for (int i = 1; i < argc; i++) {
        char* arg = argv[i];
        if (arg[0] == 0) continue;
        if (onlyFiles) {
            files.push_back(arg);
            continue;
        }
        if (arg[0] != '-') {
            files.push_back(arg);
            continue;
        }
        if (arg[1] == 0) {
            files.push_back(arg);
            continue;
        }
        if (arg[1] == '-') {
            if (arg[2] == 0)
                onlyFiles = true;
            else if (strcmp(arg + 2, "help") == 0)
                help = true;
#ifdef YES_PRETTY
            else if (strcmp(arg + 2, "pretty") == 0)
                pretty = true;
#endif
#ifdef YES_CPRETTY
            else if (strcmp(arg + 2, "cpretty") == 0)
                cpretty = true;
#endif
#ifdef YES_TREE
            else if (strcmp(arg + 2, "tree") == 0)
                tree = true;
#endif
#ifdef YES_HASKELL
            else if (strcmp(arg + 2, "haskell") == 0)
                haskell = true;
#endif
#ifdef YES_SYSTEST
            else if (strcmp(arg + 2, "systest") == 0)
                systest = true;
#endif
            else {
                cerr << "Invalid option: " << arg << endl;
                return 1;
            }
            continue;
        }
        for (char* i = arg + 1; *i; ++i) {
            switch (*i) {
                case 'h':
                    help = true;
                    break;
#ifdef YES_PRETTY
                case 'p':
                    pretty = true;
                    break;
#endif
#ifdef YES_CPRETTY
                case 'P':
                    cpretty = true;
                    break;
#endif
#ifdef YES_TREE
                case 't':
                    tree = true;
                    break;
#endif
#ifdef YES_HASKELL
                case 'H':
                    haskell = true;
                    break;
#endif
#ifdef YES_SYSTEST
                case 's':
                    systest = true;
                    break;
#endif
                default:
                    cerr << "Invalid option: -" << *i << endl;
                    return 1;
            }
        }
    }
    if (!help && files.empty())
        files.push_back("-");
    if (help) {
        cout << "Example syntax parser.\nUsage:\n"
             << argv[0] << " (OPTION|FILE)... [-- FILE...]\n";
        cout << R"%(
Options:
  -h --help     Display this message.
)%"
#ifdef YES_PRETTY
"  -p --pretty   "
"Pretty-print the syntax tree (using ContextFreePrettyPrinter).\n"
#endif
#ifdef YES_CPRETTY
"  -P --cpretty  Pretty-print the syntax tree (using ClassicPrettyPrinter).\n"
#endif
#ifdef YES_TREE
"  -t --tree     Print the abstract syntax tree like a tree.\n"
#endif
#ifdef YES_HASKELL
"  -H --haskell  Print the abstract syntax tree as a Haskell expression.\n"
#endif
#ifdef YES_SYSTEST
"  -s --systest  For use in BNFC system tests. Overrides other options.\n"
#endif
R"%(     --         Treat the remaining arguments as files.

If no files are specified of if FILE is -, read standard input.

The exit code is the number of files for which parsing failed. In particular,
if every file is parsed successfully, the exit code will be 0.
)%";
    }
    bool printFilenames = files.size() > 1;
    int errorcount = 0;
    for (const char* filename : files) {
        FILE* file; bool needclose;
        std::string sfilename;
        if (strcmp(filename, "-") == 0) {
            file = stdin;
            needclose = false;
            cerr << "Reading from stdin..." << endl;
            sfilename = "<stdin>";
        }
        else {
            file = fopen(filename, "r");
            if (!file) {
                cerr << "Cannot open file ";
                perror(filename);
                errorcount += errorcount != 0x7FFFFFFF;
                continue;
            }
            sfilename = filename;
            needclose = true;
        }
        if (printFilenames) cerr << filename << endl;

|]
  $+$ linesToText
  [ "        " ++ ns ++ "Parse(file, &sfilename) | PatternMatch{"
  , "            [&](" ++ ns ++ "Parser::syntax_error&& err) {"
  , "                cerr << \"Could not parse!\\nError: \" "
    ++ "<< err.what() << '\\n';"
  , "                cerr << \"At \" << err.location << \"\\n\\n\";"
  , "                errorcount += errorcount != 0x7FFFFFFF;"
  , "            },"
  , "            [](" ++ ns ++ "ParseResultVariant&& ast) {"
  , "#ifdef YES_SYSTEST"
  , "                if (systest) {"
  , "                    cout << \"Parse Successful!\\n\\n"
    ++ "[Abstract Syntax]\\n\\n\";"
  , "                    ast | " ++ ns ++ "HaskellPrinter(cout);"
  , "                    cout << \"\\n\\n[Linearized tree]\\n\\n\";"
  , "                    ast | " ++ ns ++ "ClassicPrettyPrinter(cout, 2);"
  , "                    cout << endl;"
  , "                    return;"
  , "                }"
  , "#endif"
  , "                bool printed = false;"
  , "#ifdef YES_TREE"
  , "                if (tree) {"
  , "                    ast | " ++ ns ++ "SyntaxPrinter(cout);"
  , "                    cout << '\\n';"
  , "                    printed = true;"
  , "                }"
  , "#endif"
  , "#ifdef YES_PRETTY"
  , "                if (pretty) {"
  , "                    ast | " ++ ns ++ "ContextFreePrettyPrinter(cout);"
  , "                    cout << \"\\n\\n\";"
  , "                    printed = true;"
  , "                }"
  , "#endif"
  , "#ifdef YES_CPRETTY"
  , "                if (cpretty) {"
  , "                    ast | " ++ ns ++ "ClassicPrettyPrinter(cout, 2);"
  , "                    cout << \"\\n\\n\";"
  , "                    printed = true;"
  , "                }"
  , "#endif"
  , "#ifdef YES_HASKELL"
  , "                if (haskell) {"
  , "                    ast | " ++ ns ++ "HaskellPrinter(cout);"
  , "                    cout << \"\\n\\n\";"
  , "                    printed = true;"
  , "                }"
  , "#endif"
  ] $+$ unlinesToText [s|
                if (!printed) cout << "OK\n\n";
                return;
            }
        };

        if (needclose) fclose(file);
    }

    return errorcount;
}
|]
