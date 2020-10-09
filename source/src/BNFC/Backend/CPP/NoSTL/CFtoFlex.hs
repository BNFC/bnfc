{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

{-
    BNF Converter: Flex generator
    Copyright (C) 2004  Author:  Michael Pellauer

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

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the Flex file. It is
                    similar to JLex but with a few peculiarities.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 5 August, 2003

    Modified      : 22 August, 2006 by Aarne Ranta


   **************************************************************
-}
module BNFC.Backend.CPP.NoSTL.CFtoFlex (cf2flex) where

import Prelude hiding ((<>))
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import BNFC.CF
import BNFC.Backend.C.CFtoFlexC (cMacros, commentStates)
import BNFC.Backend.C.RegToFlex
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.CPP.STL.STLUtils
import BNFC.PrettyPrint
import BNFC.Utils (cstring)

--The environment must be returned for the parser to use.
cf2flex :: Maybe String -> String -> CF -> (String, SymMap)
cf2flex inPackage _name cf = (, env) $ unlines
    [ prelude inPackage
    , cMacros cf
    , lexSymbols env0
    , restOfFlex inPackage cf env
    ]
  where
    env  = Map.fromList env1
    env0 = makeSymEnv (cfgSymbols cf ++ reservedWords cf) [0 :: Int ..]
    env1 = map (first Keyword) env0 ++ makeSymEnv (map Tokentype $ tokenNames cf) [length env0 ..]
    makeSymEnv = zipWith $ \ s n -> (s, "_SYMB_" ++ show n)

prelude :: Maybe String -> String
prelude inPackage = unlines
  [
   maybe "" (\ns -> "%option prefix=\"" ++ ns ++ "yy\"") inPackage,
   "/* This FLex file was machine-generated by the BNF converter */",
   "%{",
   "#include <string.h>",
   "#include \"Parser.H\"",
   "#define YY_BUFFER_LENGTH 4096",
   "extern int " ++ nsString inPackage ++ "yy_mylinenumber ;", --- hack to get line number. AR 2006
   "static char YY_PARSED_STRING[YY_BUFFER_LENGTH];",
   "static void YY_BUFFER_APPEND(const char *s)",
   "{",
   "  strcat(YY_PARSED_STRING, s); //Do something better here!",
   "}",
   "static void YY_BUFFER_RESET(void)",
   "{",
   "  memset(YY_PARSED_STRING, 0, YY_BUFFER_LENGTH);",
   "}",
   "",
   "%}"
  ]

lexSymbols :: SymEnv -> String
lexSymbols ss = concatMap transSym ss
  where
    transSym (s,r) =
      "<YYINITIAL>\"" ++ s' ++ "\"      \t return " ++ r ++ ";\n"
        where
         s' = escapeChars s

restOfFlex :: Maybe String -> CF -> SymMap -> String
restOfFlex inPackage cf env = unlines $ concat
  [ [ render $ lexComments inPackage (comments cf)
    , ""
    ]
  , userDefTokens
  , ifC catString  strStates
  , ifC catChar    chStates
  , ifC catDouble  [ "<YYINITIAL>{DIGIT}+\".\"{DIGIT}+(\"e\"(\\-)?{DIGIT}+)?      \t " ++ ns ++ "yylval._double = atof(yytext); return " ++ nsDefine inPackage "_DOUBLE_" ++ ";" ]
  , ifC catInteger [ "<YYINITIAL>{DIGIT}+      \t " ++ ns ++ "yylval._int = atoi(yytext); return " ++ nsDefine inPackage "_INTEGER_" ++ ";" ]
  , ifC catIdent   [ "<YYINITIAL>{LETTER}{IDENT}*      \t " ++ ns ++ "yylval._string = strdup(yytext); return " ++ nsDefine inPackage "_IDENT_" ++ ";" ]
  , [ "\\n  ++" ++ ns ++ "yy_mylinenumber ;"
    , "<YYINITIAL>[ \\t\\r\\n\\f]      \t /* ignore white space. */;"
    , "<YYINITIAL>.      \t return " ++ nsDefine inPackage "_ERROR_" ++ ";"
    , "%%"
    ]
  , footer
  ]
  where
   ifC cat s = if isUsedCat Parsable cf (TokenCat cat) then s else []
   ns = nsString inPackage
   userDefTokens =
     [ "<YYINITIAL>" ++ printRegFlex exp ++
         "     \t " ++ ns ++ "yylval._string = strdup(yytext); return " ++ sName name ++ ";"
     | (name, exp) <- tokenPragmas cf
     ]
     where sName n = fromMaybe n $ Map.lookup (Tokentype n) env
   strStates =  --These handle escaped characters in Strings.
     [ "<YYINITIAL>\"\\\"\"      \t BEGIN STRING;"
     , "<STRING>\\\\      \t BEGIN ESCAPED;"
     , "<STRING>\\\"      \t " ++ ns ++ "yylval._string = strdup(YY_PARSED_STRING); YY_BUFFER_RESET(); BEGIN YYINITIAL; return " ++ nsDefine inPackage "_STRING_" ++ ";"
     , "<STRING>.      \t YY_BUFFER_APPEND(yytext);"
     , "<ESCAPED>n      \t YY_BUFFER_APPEND(\"\\n\"); BEGIN STRING;"
     , "<ESCAPED>\\\"      \t YY_BUFFER_APPEND(\"\\\"\"); BEGIN STRING ;"
     , "<ESCAPED>\\\\      \t YY_BUFFER_APPEND(\"\\\\\"); BEGIN STRING;"
     , "<ESCAPED>t       \t YY_BUFFER_APPEND(\"\\t\"); BEGIN STRING;"
     , "<ESCAPED>.       \t YY_BUFFER_APPEND(yytext); BEGIN STRING;"
     ]
   chStates = --These handle escaped characters in Chars.
     [ "<YYINITIAL>\"'\" \tBEGIN CHAR;"
     , "<CHAR>\\\\      \t BEGIN CHARESC;"
     , "<CHAR>[^']      \t BEGIN CHAREND; " ++ ns ++ "yylval._char = yytext[0]; return " ++ nsDefine inPackage "_CHAR_" ++ ";"
     , "<CHARESC>n      \t BEGIN CHAREND; " ++ ns ++ "yylval._char = '\\n'; return " ++ nsDefine inPackage "_CHAR_" ++ ";"
     , "<CHARESC>t      \t BEGIN CHAREND; " ++ ns ++ "yylval._char = '\\t'; return " ++ nsDefine inPackage "_CHAR_" ++ ";"
     , "<CHARESC>.      \t BEGIN CHAREND; " ++ ns ++ "yylval._char = yytext[0]; return " ++ nsDefine inPackage "_CHAR_" ++ ";"
     , "<CHAREND>\"'\"      \t BEGIN YYINITIAL;"
     ]
   footer =
     [ "void " ++ ns ++ "initialize_lexer(FILE *inp) { yyrestart(inp); BEGIN YYINITIAL; }"
     , "int yywrap(void) { return 1; }"
     ]


-- ---------------------------------------------------------------------------
-- Comments

-- | Create flex rules for single-line and multi-lines comments.
-- The first argument is an optional namespace (for C++); the second
-- argument is the set of comment delimiters as returned by BNFC.CF.comments.
--
-- This function is only compiling the results of applying either
-- lexSingleComment or lexMultiComment on each comment delimiter or pair of
-- delimiters.
--
-- >>> lexComments (Just "myns.") ([("{-","-}")],["--"])
-- <YYINITIAL>"--"[^\n]* ; // BNFC: comment "--";
-- <YYINITIAL>"{-" BEGIN COMMENT; // BNFC: block comment "{-" "-}";
-- <COMMENT>"-}" BEGIN YYINITIAL;
-- <COMMENT>.    /* skip */;
-- <COMMENT>[\n] ++myns.yy_mylinenumber;
lexComments :: Maybe String -> ([(String, String)], [String]) -> Doc
lexComments ns (m,s) = vcat $ concat
  [ map    (lexSingleComment ns) s
  , zipWith (lexMultiComment ns) m commentStates
  ]

-- | Create a lexer rule for single-line comments.
-- The first argument is -- an optional c++ namespace
-- The second argument is the delimiter that marks the beginning of the
-- comment.
--
-- >>> lexSingleComment (Just "mypackage.") "--"
-- <YYINITIAL>"--"[^\n]* ; // BNFC: comment "--";
--
-- >>> lexSingleComment Nothing "--"
-- <YYINITIAL>"--"[^\n]* ; // BNFC: comment "--";
--
-- >>> lexSingleComment Nothing "\""
-- <YYINITIAL>"\""[^\n]* ; // BNFC: comment "\"";
lexSingleComment :: Maybe String -> String -> Doc
lexSingleComment _ c =
    "<YYINITIAL>" <> cstring c <> "[^\\n]*"
    <+> ";"
    <+> "// BNFC: comment" <+> cstring c <> ";"

-- -- | Create a lexer rule for single-line comments.
-- -- The first argument is -- an optional c++ namespace
-- -- The second argument is the delimiter that marks the beginning of the
-- -- comment.
-- --
-- -- >>> lexSingleComment (Just "mypackage.") "--"
-- -- <YYINITIAL>"--"[^\n]*\n ++mypackage.yy_mylinenumber; // BNFC: comment "--";
-- --
-- -- >>> lexSingleComment Nothing "--"
-- -- <YYINITIAL>"--"[^\n]*\n ++yy_mylinenumber; // BNFC: comment "--";
-- --
-- -- >>> lexSingleComment Nothing "\""
-- -- <YYINITIAL>"\""[^\n]*\n ++yy_mylinenumber; // BNFC: comment "\"";
-- lexSingleComment :: Maybe String -> String -> Doc
-- lexSingleComment ns c =
--     "<YYINITIAL>" <> cstring c <> "[^\\n]*\\n"
--     <+> "++"<> text (fromMaybe "" ns)<>"yy_mylinenumber;"
--     <+> "// BNFC: comment" <+> cstring c <> ";"

-- | Create a lexer rule for multi-lines comments.
-- The first argument is -- an optional c++ namespace
-- The second arguments is the pair of delimiter for the multi-lines comment:
-- start deleminiter and end delimiter.
-- There might be a possible bug here if a language includes 2 multi-line
-- comments. They could possibly start a comment with one character and end it
-- with another.  However this seems rare.
--
-- >>> lexMultiComment Nothing ("{-", "-}") "COMMENT"
-- <YYINITIAL>"{-" BEGIN COMMENT; // BNFC: block comment "{-" "-}";
-- <COMMENT>"-}" BEGIN YYINITIAL;
-- <COMMENT>.    /* skip */;
-- <COMMENT>[\n] ++yy_mylinenumber;
--
-- >>> lexMultiComment (Just "foo.") ("{-", "-}") "COMMENT"
-- <YYINITIAL>"{-" BEGIN COMMENT; // BNFC: block comment "{-" "-}";
-- <COMMENT>"-}" BEGIN YYINITIAL;
-- <COMMENT>.    /* skip */;
-- <COMMENT>[\n] ++foo.yy_mylinenumber;
--
-- >>> lexMultiComment Nothing ("\"'", "'\"") "COMMENT"
-- <YYINITIAL>"\"'" BEGIN COMMENT; // BNFC: block comment "\"'" "'\"";
-- <COMMENT>"'\"" BEGIN YYINITIAL;
-- <COMMENT>.    /* skip */;
-- <COMMENT>[\n] ++yy_mylinenumber;
lexMultiComment :: Maybe String -> (String, String) -> String -> Doc
lexMultiComment ns (b,e) comment = vcat
    [ "<YYINITIAL>" <> cstring b <+> "BEGIN" <+> text comment <> ";"
        <+> "// BNFC: block comment" <+> cstring b <+> cstring e <> ";"
    , commentTag <> cstring e <+> "BEGIN YYINITIAL;"
    , commentTag <> ".    /* skip */;"
    , commentTag <> "[\\n] ++" <> text (fromMaybe "" ns) <> "yy_mylinenumber;"
    ]
  where
  commentTag = text $ "<" ++ comment ++ ">"

-- | Helper function that escapes characters in strings.
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : ('\\' : (escapeChars xs))
escapeChars ('\"':xs) = '\\' : ('\"' : (escapeChars xs))
escapeChars (x:xs) = x : (escapeChars xs)
