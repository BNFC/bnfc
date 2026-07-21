{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.PrinterCommonGen
  Description : Generates C++ utils that all printers use.

  Generates C++ utils that all printers use.
-}

module BNFC.Backend.CPPVar.PrinterCommonGen
  (
    -- * The entrypoints
    makePrinterCommonHpp
  , makePrinterCommonCpp

    -- * File naming
  , printerCommonHppFilename
  , printerCommonCppFilename
  ) where

-- Language imports
import Data.String.QQ (s)

import Text.PrettyPrint (Doc)

-- BNFC imports
import qualified BNFC.Options as Options
import BNFC.Backend.CPPVar.CPPUtil

------------------------------------------------------------------------
-- * File naming.
------------------------------------------------------------------------

-- | The name of the header file.
printerCommonHppFilename :: String
printerCommonHppFilename = "PrinterCommon.hpp"

-- | The name of the source file.
printerCommonCppFilename :: String
printerCommonCppFilename = "PrinterCommon.cpp"

------------------------------------------------------------------------
-- * The entrypoints.
------------------------------------------------------------------------

-- | Generates the header file included by printers.
makePrinterCommonHpp ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> Doc
makePrinterCommonHpp opts = linesToText
  [ "#pragma once"
  , "#include <ostream>"
  , "#include <string_view>"
  ] $++$ (nsutils_wrap $ newNamespaceUtilsFromOptions opts) (unlinesToText [s|
void PrintEscapedCharRaw(std::ostream& out, int32_t ch);

// As PrintEscapedCharRaw for each character,
// but also escapes double quotes (").
// Puts the string in double quotes.
void PrintEscapedString(std::ostream& out, std::string_view s);

// As PrintEscapedCharRaw, but also escapes single quotes (').
// Puts the character in single quotes.
void PrintEscapedChar(std::ostream& out, int32_t ch);

void PrintDouble(std::ostream& out, double val);
|])

-- | Generates the implementations for printing utils.
makePrinterCommonCpp ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> Doc
makePrinterCommonCpp opts = linesToText (lines $ [s|
#include "PrinterCommon.hpp"
#include <charconv>
#include <system_error>
|]) $++$ (nsutils_wrap $ newNamespaceUtilsFromOptions opts) (unlinesToText [s|
void PrintEscapedCharRaw(std::ostream& out, int32_t ch) {
    switch (ch) {
        case '\0':
            out << "\\0";
            break;
        case '\a':
            out << "\\a";
            break;
        case '\b':
            out << "\\b";
            break;
        case '\f':
            out << "\\f";
            break;
        case '\n':
            out << "\\n";
            break;
        case '\r':
            out << "\\r";
            break;
        case '\t':
            out << "\\t";
            break;
        case '\v':
            out << "\\v";
            break;
        case '\\':
            out << "\\\\";
            break;
        default:
            if (0x20 <= ch && ch < 0x7F) {
                out << static_cast<char>(ch);
                break;
            }
            int digitcount;
            if (0 <= ch && ch <= 0xFF) {
                out << "\\x";
                digitcount = 2;
            } else if (0 <= ch && ch <= 0xFFFF) {
                out << "\\u";
                digitcount = 4;
            } else {
                out << "\\U";
                digitcount = 8;
            }
            for (int i = (digitcount - 1) * 4; i >= 0; i -= 4)
                out << "0123456789ABCDEF"[(ch >> i) & 0xF];
            break;
    }
}

void PrintEscapedString(std::ostream& out, std::string_view s) {
    out << '"';
    for (char ch : s) {
        if (ch < 0)  // do not touch utf-8 non-ascii
            out << ch;
        else if (ch == '"')
            out << "\\\"";
        else
            PrintEscapedCharRaw(out, ch);
    }
    out << '"';
}

void PrintEscapedChar(std::ostream& out, int32_t ch) {
    out << '\'';
    if (ch < 0)
        out << "BAD_CHAR";
    else if (ch == static_cast<int32_t>('\''))
        out << "\\'";
    else
        PrintEscapedCharRaw(out, ch);
    out << '\'';
}

void PrintDouble(std::ostream& out, double val) {
    char buf[128];
    auto res = std::to_chars(buf, buf + sizeof(buf), val);
    if (res.ec != std::errc())
        throw std::system_error(std::make_error_code(res.ec));
    out << std::string_view(buf, res.ptr - buf);
}
|])
