{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-
    BNF Converter: C flex generator
    Copyright (C) 2004  Author:  Michael Pellauer
    Copyright (C) 2020  Andreas Abel

    Description   : This module generates the Flex file. It is
                    similar to JLex but with a few peculiarities.

    Author        : Michael Pellauer
    Created       : 5 August, 2003
-}

module BNFC.Backend.C.CFtoFlexC
  ( cf2flex
  , ParserMode(..), parserName, parserPackage, reentrant, cParser, stlParser, parserHExt, variant, beyondAnsi, isBisonUseUnion, isBisonUseVariant
  , preludeForBuffer  -- C code defining a buffer for lexing string literals.
  , cMacros           -- Lexer definitions.
  , commentStates     -- Stream of names for lexer states for comments.
  , lexComments       -- Lexing rules for comments.
  , lexStrings        -- Lexing rules for string literals.
  , lexChars          -- Lexing rules for character literals.
  ) where

import Prelude hiding                ( (<>) )
import Data.Bifunctor                ( first )
import Data.Char                     ( isAlphaNum, isAscii )
import Data.List                     ( isInfixOf )
import Data.Maybe                    ( fromMaybe, maybeToList )
import qualified Data.Map as Map
import System.FilePath               ( (<.>) )

import BNFC.CF
import BNFC.Backend.C.Common         ( posixC )
import BNFC.Backend.C.RegToFlex
import BNFC.Backend.Common.NamedVariables
import BNFC.Options                  ( InPackage, Ansi(..) )
import BNFC.PrettyPrint
import BNFC.Utils                    ( cstring, symbolToName, unless, when, camelCase_ )

data ParserMode
  = CParser Bool String             -- ^ @C@ (@False@) or @C++ no STL@ (@True@) mode, with @name@ to use as prefix.
  | CppParser InPackage String Ansi -- ^ @C++@ mode, with optional package name, --ansi or -std=c++14

parserName :: ParserMode -> String
parserName = \case
  CParser   _ n -> n
  CppParser p n _ -> fromMaybe n p

parserPackage :: ParserMode -> InPackage
parserPackage = \case
  CParser   _ _ -> Nothing
  CppParser p _ _ -> p

reentrant :: ParserMode -> String
reentrant = \case
  CParser   _ _ -> "%pure_parser";
  CppParser _ _ ansi | ansi == BeyondAnsi -> "/* \"lalr1.cc\" is always pure parser. needless to define %define api.pure full */"
                     | otherwise          -> "%pure_parser";

variant :: ParserMode -> [String]
variant = \case
  CppParser _ _ ansi | ansi == BeyondAnsi -> [
                         "/* variant based implementation of semantic values for C++ */"
                         ,"%require \"3.2\""
                         ,"%define api.value.type variant"
                         ,"/* 'yacc.c' does not support variant, so use skeleton 'lalr1.cc' */"
                         ,"%skeleton \"lalr1.cc\""]
  _ -> []

beyondAnsi :: ParserMode -> Bool
beyondAnsi = \case
  CppParser _ _ ansi | ansi == BeyondAnsi -> True
  _ -> False

isBisonUseUnion :: ParserMode -> Bool
isBisonUseUnion = \case
  CppParser _ _ ansi | ansi == Ansi -> True
  _ -> False

isBisonUseVariant :: ParserMode -> Bool
isBisonUseVariant = \case
  CppParser _ _ ansi | ansi == BeyondAnsi -> True
  _ -> False

cParser :: ParserMode -> Bool
cParser = \case
  CParser   b _ -> not b
  CppParser _ _ _ -> False

stlParser :: ParserMode -> Bool
stlParser = \case
  CParser   _ _ -> False
  CppParser _ _ _ -> True

parserHExt :: ParserMode -> String
parserHExt = \case
  CParser   b _ -> if b then "H" else "h"
  CppParser _ _ ansi | ansi == BeyondAnsi -> "hh"
                     | otherwise -> "h"

-- | Entrypoint.
cf2flex :: ParserMode -> CF -> (String, SymMap) -- The environment is reused by the parser.
cf2flex mode cf = (, env) $ unlines
    [ prelude stringLiterals mode
    , cMacros cf
    , lexSymbols env1
    , restOfFlex (parserPackage mode) cf env
    , footer -- mode
    ]
  where
    env  = Map.fromList env2
    env0 = makeSymEnv (cfgSymbols cf) [0 :: Int ..]
    env1 = env0 ++ makeKwEnv (reservedWords cf) [length env0 ..]
    env2 = map (first Keyword) env1 ++ map (\ x -> (Tokentype x, "T_" ++ x)) (tokenNames cf)
    makeSymEnv     = zipWith $ \ s n -> (s, '_' : fromMaybe ("SYMB_" ++ show n) (symbolToName s))
    makeKwEnv      = zipWith $ \ s n -> (s, "_KW_" ++ if all (\ c -> isAlphaNum c && isAscii c) s then s else show n)
    stringLiterals = isUsedCat cf (TokenCat catString)

prelude :: Bool -> ParserMode -> String
prelude stringLiterals mode = unlines $ concat
  [ [ "/* Lexer definition for use with FLex */"
    , ""
    , if (beyondAnsi mode) then
        unlines
        [
          "%option nodefault noyywrap c++"
        , "%option yyclass=\"" ++ns++ "::" ++camelCaseName++ "Scanner\""
        ]
      else
        unlines
        -- noinput and nounput are most often unused
        -- https://stackoverflow.com/questions/39075510/option-noinput-nounput-what-are-they-for
        [ "%option noyywrap noinput nounput"
        , "%option reentrant bison-bridge bison-locations"
        , ""
        ]
    ]
  , when stringLiterals
    [ "/* Additional data for the lexer: a buffer for lexing string literals. */"
    , "%option extra-type=\"Buffer\""
    , ""
    ]
  , maybeToList $ ("%option prefix=\"" ++) . (++ "\"" ) <$> parserPackage mode
  , when (cParser mode) $ concat
    -- The following #define needs to be at the top before the automatic #include <stdlib.h>
    [ [ "%top{" ]
    , posixC
    , [ "}" ]
    ]
  , when (beyondAnsi mode)
    [ "%top{"
    , "#include <memory>"
    , "}"
    ]
  , [ "%{"
    , when (beyondAnsi mode) unlines
      [
        "#include \"Scanner.hh\""  -- #include for the class inheriting "yyFlexLexer"
      , ""
      , "#undef  YY_DECL"
      , "#define YY_DECL int " ++ns++ "::" ++camelCaseName++ "Scanner::yylex(" ++ns++ "::" ++camelCaseName++ "Parser::semantic_type* const lval, " ++ns++ "::" ++camelCaseName++ "Parser::location_type* location )"
      ]
    , "#include \"" ++ ("Absyn" <.> h) ++ "\""
    , "#include \"" ++ ("Bison" <.> h) ++ "\""
    , ""
    ]
  , [ "#define initialize_lexer " ++ parserName mode ++ "_initialize_lexer"
    , ""
    ]
  , when stringLiterals $ preludeForBuffer $ "Buffer" <.> h
    -- https://www.gnu.org/software/bison/manual/html_node/Token-Locations.html
    -- Flex is responsible for keeping tracking of the yylloc for Bison.
    -- Flex also doesn't do this automatically so we need this function
    -- https://stackoverflow.com/a/22125500/425756
  , if beyondAnsi mode then
      [ "/* update location on matching */"
      , "#define YY_USER_ACTION loc->step(); loc->columns(yyleng);"
      , "%}"
      ]
    else
      [ "static void update_loc(YYLTYPE* loc, char* text)"
      , "{"
      , "  loc->first_line = loc->last_line;"
      , "  loc->first_column = loc->last_column;"
      , "  int i = 0;"  -- put this here as @for (int i...)@ is only allowed in C99
      , "  for (; text[i] != '\\0'; ++i) {"
      , "      if (text[i] == '\\n') {"        -- Checking for \n is good enough to also support \r\n (but not \r)
      , "          ++loc->last_line;"
      , "          loc->last_column = 0; "
      , "      } else {"
      , "          ++loc->last_column; "
      , "      }"
      , "  }"
      , "}"
      , "#define YY_USER_ACTION update_loc(yylloc, yytext);"
      , ""
      , "%}"
      ]
  ]
  where
    h = parserHExt mode
    name = parserName mode
    camelCaseName = camelCase_ name
    ns = fromMaybe camelCaseName (parserPackage mode)

-- | Part of the lexer prelude needed when string literals are to be lexed.
--   Defines an interface to the Buffer.
preludeForBuffer :: String -> [String]
preludeForBuffer bufferH =
    [ "/* BEGIN extensible string buffer */"
    , ""
    , "#include \"" ++ bufferH ++ "\""
    , ""
    , "/* The initial size of the buffer to lex string literals. */"
    , "#define LITERAL_BUFFER_INITIAL_SIZE 1024"
    , ""
    , "/* The pointer to the literal buffer. */"
    , "#define literal_buffer yyextra"
    , ""
    , "/* Initialize the literal buffer. */"
    , "#define LITERAL_BUFFER_CREATE() literal_buffer = newBuffer(LITERAL_BUFFER_INITIAL_SIZE)"
    , ""
    , "/* Append characters at the end of the buffer. */"
    , "#define LITERAL_BUFFER_APPEND(s) bufferAppendString(literal_buffer, s)"
    , ""
    , "/* Append a character at the end of the buffer. */"
    , "#define LITERAL_BUFFER_APPEND_CHAR(c) bufferAppendChar(literal_buffer, c)"
    , ""
    , "/* Release the buffer, returning a pointer to its content. */"
    , "#define LITERAL_BUFFER_HARVEST() releaseBuffer(literal_buffer)"
    , ""
    , "/* In exceptional cases, e.g. when reaching EOF, we have to free the buffer. */"
    , "#define LITERAL_BUFFER_FREE() freeBuffer(literal_buffer)"
    , ""
    , "/* END extensible string buffer */"
    , ""
    ]

-- For now all categories are included.
-- Optimally only the ones that are used should be generated.
cMacros :: CF ->  String
cMacros cf = unlines
  [ "LETTER [a-zA-Z]"
  , "CAPITAL [A-Z]"
  , "SMALL [a-z]"
  , "DIGIT [0-9]"
  , "IDENT [a-zA-Z0-9'_]"
  , unwords $ concat
      [ [ "%START CHAR CHARESC CHAREND STRING ESCAPED" ]
      , take (numberOfBlockCommentForms cf) commentStates
      ]
  , ""
  , "%%  /* Rules. */"
  ]

lexSymbols :: KeywordEnv -> String
lexSymbols ss = concatMap transSym ss
  where
    transSym (s,r) =
      "<INITIAL>\"" ++ s' ++ "\"      \t return " ++ r ++ ";\n"
        where
         s' = escapeChars s

restOfFlex :: InPackage -> CF -> SymMap -> String
restOfFlex _inPackage cf env = unlines $ concat
  [ [ render $ lexComments $ comments cf
    , ""
    ]
  , userDefTokens
  , ifC catString  $ lexStrings "yylval" "_STRING_" "_ERROR_"
  , ifC catChar    $ lexChars   "yylval" "_CHAR_"
  , ifC catDouble  [ "<INITIAL>{DIGIT}+\".\"{DIGIT}+(\"e\"(\\-)?{DIGIT}+)?      \t yylval->_double = atof(yytext); return _DOUBLE_;" ]
  , ifC catInteger [ "<INITIAL>{DIGIT}+      \t yylval->_int = atoi(yytext); return _INTEGER_;" ]
  , ifC catIdent   [ "<INITIAL>{LETTER}{IDENT}*      \t yylval->_string = strdup(yytext); return _IDENT_;" ]
  , [ "<INITIAL>[ \\t\\r\\n\\f]      \t /* ignore white space. */;"
    , "<INITIAL>.      \t return _ERROR_;"
    , ""
    , "%%  /* Initialization code. */"
    ]
  ]
  where
  ifC cat s = if isUsedCat cf (TokenCat cat) then s else []
  userDefTokens =
    [ "<INITIAL>" ++ printRegFlex exp ++
       "    \t yylval->_string = strdup(yytext); return " ++ sName name ++ ";"
    | (name, exp) <- tokenPragmas cf
    ]
    where sName n = fromMaybe n $ Map.lookup (Tokentype n) env

footer :: String
footer = unlines
    [ "yyscan_t initialize_lexer(FILE *inp)"
    , "{"
    , "  yyscan_t scanner;"
    , "  if (yylex_init_extra(NULL, &scanner)) return 0;"
    , "  if (inp) yyrestart(inp, scanner);"
    , "  return scanner;"
    , "}"
    ]

-- | Lexing of strings, converting escaped characters.
lexStrings :: String -> String -> String -> [String]
lexStrings yylval stringToken errorToken =
    [ "<INITIAL>\"\\\"\"        \t LITERAL_BUFFER_CREATE(); BEGIN STRING;"
    , "<STRING>\\\\             \t BEGIN ESCAPED;"
    , "<STRING>\\\"             \t " ++ yylval ++ "->_string = LITERAL_BUFFER_HARVEST(); BEGIN INITIAL; return " ++ stringToken ++ ";"
    , "<STRING>.              \t LITERAL_BUFFER_APPEND_CHAR(yytext[0]);"
    , "<ESCAPED>f             \t LITERAL_BUFFER_APPEND_CHAR('\\f'); BEGIN STRING;"
    , "<ESCAPED>n             \t LITERAL_BUFFER_APPEND_CHAR('\\n'); BEGIN STRING;"
    , "<ESCAPED>r             \t LITERAL_BUFFER_APPEND_CHAR('\\r'); BEGIN STRING;"
    , "<ESCAPED>t             \t LITERAL_BUFFER_APPEND_CHAR('\\t'); BEGIN STRING;"
    , "<ESCAPED>\\\"            \t LITERAL_BUFFER_APPEND_CHAR('\"');  BEGIN STRING;"
    , "<ESCAPED>\\\\            \t LITERAL_BUFFER_APPEND_CHAR('\\\\'); BEGIN STRING;"
    , "<ESCAPED>.             \t LITERAL_BUFFER_APPEND(yytext);    BEGIN STRING;"
    , "<STRING,ESCAPED><<EOF>>\t LITERAL_BUFFER_FREE(); return " ++ errorToken ++ ";"
    ]

-- | Lexing of characters, converting escaped characters.
lexChars :: String -> String -> [String]
lexChars yylval charToken =
    [ "<INITIAL>\"'\" \tBEGIN CHAR;"
    , "<CHAR>\\\\      \t BEGIN CHARESC;"
    , "<CHAR>[^']      \t BEGIN CHAREND; " ++ yylval ++ "->_char = yytext[0]; return " ++ charToken ++ ";"
    , "<CHARESC>f      \t BEGIN CHAREND; " ++ yylval ++ "->_char = '\\f';     return " ++ charToken ++ ";"
    , "<CHARESC>n      \t BEGIN CHAREND; " ++ yylval ++ "->_char = '\\n';     return " ++ charToken ++ ";"
    , "<CHARESC>r      \t BEGIN CHAREND; " ++ yylval ++ "->_char = '\\r';     return " ++ charToken ++ ";"
    , "<CHARESC>t      \t BEGIN CHAREND; " ++ yylval ++ "->_char = '\\t';     return " ++ charToken ++ ";"
    , "<CHARESC>.      \t BEGIN CHAREND; " ++ yylval ++ "->_char = yytext[0]; return " ++ charToken ++ ";"
    , "<CHAREND>\"'\"      \t BEGIN INITIAL;"
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
-- >>> lexComments ([("{-","-}")],["--"])
-- <INITIAL>"--"[^\n]* /* skip */; /* BNFC: comment "--" */
-- <INITIAL>"{-" BEGIN COMMENT; /* BNFC: block comment "{-" "-}" */
-- <COMMENT>"-}" BEGIN INITIAL;
-- <COMMENT>.    /* skip */;
-- <COMMENT>[\n] /* skip */;
lexComments :: ([(String, String)], [String]) -> Doc
lexComments (m,s) = vcat $ concat
  [ map lexSingleComment s
  , zipWith lexMultiComment m commentStates
  ]

-- | If we have several block comments, we need different COMMENT lexing states.
commentStates :: [String]
commentStates = map ("COMMENT" ++) $ "" : map show [1..]

-- | Create a lexer rule for single-line comments.
-- The first argument is -- an optional c++ namespace
-- The second argument is the delimiter that marks the beginning of the
-- comment.
--
-- >>> lexSingleComment "--"
-- <INITIAL>"--"[^\n]* /* skip */; /* BNFC: comment "--" */
--
-- >>> lexSingleComment "\""
-- <INITIAL>"\""[^\n]* /* skip */; /* BNFC: comment "\"" */
lexSingleComment :: String -> Doc
lexSingleComment c =
    "<INITIAL>" <> cstring c <> "[^\\n]*"
    <+> "/* skip */;"
    <+> unless (containsCCommentMarker c) ("/* BNFC: comment" <+> cstring c <+> "*/")

containsCCommentMarker :: String -> Bool
containsCCommentMarker s = "/*" `isInfixOf` s || "*/" `isInfixOf` s

-- | Create a lexer rule for multi-lines comments.
-- The first argument is -- an optional c++ namespace
-- The second arguments is the pair of delimiter for the multi-lines comment:
-- start deleminiter and end delimiter.
-- There might be a possible bug here if a language includes 2 multi-line
-- comments. They could possibly start a comment with one character and end it
-- with another.  However this seems rare.
--
-- >>> lexMultiComment ("{-", "-}") "COMMENT"
-- <INITIAL>"{-" BEGIN COMMENT; /* BNFC: block comment "{-" "-}" */
-- <COMMENT>"-}" BEGIN INITIAL;
-- <COMMENT>.    /* skip */;
-- <COMMENT>[\n] /* skip */;
--
-- >>> lexMultiComment ("\"'", "'\"") "COMMENT"
-- <INITIAL>"\"'" BEGIN COMMENT; /* BNFC: block comment "\"'" "'\"" */
-- <COMMENT>"'\"" BEGIN INITIAL;
-- <COMMENT>.    /* skip */;
-- <COMMENT>[\n] /* skip */;
lexMultiComment :: (String, String) -> String -> Doc
lexMultiComment (b,e) comment = vcat
    [ "<INITIAL>" <> cstring b <+> "BEGIN" <+> text comment <> ";"
      <+> unless (containsCCommentMarker b || containsCCommentMarker e)
          ("/* BNFC: block comment" <+> cstring b <+> cstring e <+> "*/")
    , commentTag <> cstring e <+> "BEGIN INITIAL;"
    , commentTag <> ".    /* skip */;"
    , commentTag <> "[\\n] /* skip */;"
    ]
  where
  commentTag = text $ "<" ++ comment ++ ">"

-- | Helper function that escapes characters in strings.
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : ('\\' : (escapeChars xs))
escapeChars ('\"':xs) = '\\' : ('\"' : (escapeChars xs))
escapeChars (x:xs) = x : (escapeChars xs)
