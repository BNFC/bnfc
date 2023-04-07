{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-
    BNF Converter: Java JLex generator
    Copyright (C) 2004  Author:  Michael Pellauer

    Description   : This module generates the JLex input file. This
                    file is quite different than Alex or Flex.

    Author        : Michael Pellauer
                    Bjorn Bringert

    Created       : 25 April, 2003
    Modified      : 4 Nov, 2004

-}

module BNFC.Backend.Java.CFtoJLex15 ( cf2jlex ) where

import Prelude hiding ((<>))

import BNFC.CF
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.C.CFtoFlexC           ( commentStates )
import BNFC.Backend.Java.RegToJLex
import BNFC.Options                       ( JavaLexerParser(..), RecordPositions(..) )
import BNFC.Utils                         ( cstring )

import Text.PrettyPrint

-- | The environment is returned for further use in the parser.
cf2jlex :: JavaLexerParser -> RecordPositions -> String -> CF -> (Doc, SymEnv)
cf2jlex jflex rp packageBase cf = (, env) . vcat $
  [ prelude jflex rp packageBase
  , cMacros cf
  , lexSymbols jflex env
  , restOfJLex jflex rp cf
  ]
  where
  env = zipWith (\ s n -> (s, "_SYMB_" ++ show n)) (cfgSymbols cf ++ reservedWords cf) [(0 :: Int)..]

-- | File prelude.
prelude :: JavaLexerParser -> RecordPositions -> String -> Doc
prelude jflex rp packageBase = vcat
    [ hsep [ "// Lexer definition for use with", lexerName ]
    , "package" <+> text packageBase <> ";"
    , ""
    , "import java_cup.runtime.*;"
    , "%%"
    , "%cup"
    , "%unicode"
    , if rp == RecordPositions
      then vcat
        [ "%line"
        , if jflex == JFlexCup then "%column" else ""
        , "%char" ]
      else ""
    , "%public"
    , "%{"
    , nest 2 $ vcat
        [ "String pstring = new String();"
        , "final int unknown = -1;"
        , "ComplexSymbolFactory.Location left = new ComplexSymbolFactory.Location(unknown, unknown);"
        , "ComplexSymbolFactory cf = new ComplexSymbolFactory();"
        , "public SymbolFactory getSymbolFactory() { return cf; }"
        , positionDeclarations
        , "public int line_num() { return (yyline+1); }"
        , "public ComplexSymbolFactory.Location left_loc() {"
        , if rp == RecordPositions
            then "  return new ComplexSymbolFactory.Location(yyline+1, yycolumn+1, yychar);"
            else "  return left;"
        , "}"
        , "public ComplexSymbolFactory.Location right_loc() {"
        , "  ComplexSymbolFactory.Location left = left_loc();"
        , if rp == RecordPositions
            then "return new ComplexSymbolFactory.Location(left.getLine(), left.getColumn()+yylength(), left.getOffset()+yylength());"
            else "return left;"
        , "}"
        , "public String buff()" <+> braces
            (if jflex == JFlexCup
            then "return new String(zzBuffer,zzCurrentPos,10).trim();"
            else "return new String(yy_buffer,yy_buffer_index,10).trim();")
        ]
    , "%}"
    , if jflex /= JFlexCup then vcat ["%eofval{"
      , "  return cf.newSymbol(\"EOF\", sym.EOF, left_loc(), left_loc());"
      , "%eofval}"]
        else ""
    ]
  where
    lexerName = case jflex of
      JFlexCup -> "JFlex"
      JLexCup  -> "JLex"
      Antlr4   -> undefined
    positionDeclarations
      -- JFlex always defines yyline, yychar, yycolumn, even if unused.
      | jflex == JFlexCup     = ""
      | rp == RecordPositions = "int yycolumn = unknown - 1;"
      | otherwise             = vcat
            -- subtract one so that one based numbering still ends up with unknown.
            [ "int yyline = unknown - 1;"
            , "int yycolumn = unknown - 1;"
            , "int yychar = unknown;"
            ]

--For now all categories are included.
--Optimally only the ones that are used should be generated.
cMacros :: CF -> Doc
cMacros cf = vcat $ concat
  [ [ "LETTER = ({CAPITAL}|{SMALL})"
    , "CAPITAL = [A-Z\\xC0-\\xD6\\xD8-\\xDE]"
    , "SMALL = [a-z\\xDF-\\xF6\\xF8-\\xFF]"
    , "DIGIT = [0-9]"
    , "IDENT = ({LETTER}|{DIGIT}|['_])"
    ]
  , map (text . ("%state " ++)) $ take (numberOfBlockCommentForms cf) commentStates
  , [ "%state CHAR"
    , "%state CHARESC"
    , "%state CHAREND"
    , "%state STRING"
    , "%state ESCAPED"
    , "%%"
    ]
  ]

-- |
-- >>> lexSymbols JLexCup [("foo","bar")]
-- <YYINITIAL>foo { return cf.newSymbol("", sym.bar, left_loc(), right_loc()); }
--
-- >>> lexSymbols JLexCup [("\\","bar")]
-- <YYINITIAL>\\ { return cf.newSymbol("", sym.bar, left_loc(), right_loc()); }
--
-- >>> lexSymbols JLexCup [("/","bar")]
-- <YYINITIAL>/ { return cf.newSymbol("", sym.bar, left_loc(), right_loc()); }
--
-- >>> lexSymbols JFlexCup [("/","bar")]
-- <YYINITIAL>\/ { return cf.newSymbol("", sym.bar, left_loc(), right_loc()); }
--
-- >>> lexSymbols JFlexCup [("~","bar")]
-- <YYINITIAL>\~ { return cf.newSymbol("", sym.bar, left_loc(), right_loc()); }
--
lexSymbols :: JavaLexerParser -> SymEnv -> Doc
lexSymbols jflex ss = vcat $  map transSym ss
  where
    transSym (s,r) =
      "<YYINITIAL>" <> text (escapeChars s) <> " { return cf.newSymbol(\"\", sym."
      <> text r <> ", left_loc(), right_loc()); }"
    --Helper function that escapes characters in strings
    escapeChars :: String -> String
    escapeChars = concatMap (escapeChar jflex)

restOfJLex :: JavaLexerParser -> RecordPositions -> CF -> Doc
restOfJLex jflex rp cf = vcat
    [ lexComments (comments cf)
    , ""
    , userDefTokens
    , ifC catString strStates
    , ifC catChar chStates
    , ifC catDouble
        "<YYINITIAL>{DIGIT}+\".\"{DIGIT}+(\"e\"(\\-)?{DIGIT}+)? { return cf.newSymbol(\"\", sym._DOUBLE_, left_loc(), right_loc(), Double.valueOf(yytext())); }"
    , ifC catInteger
        "<YYINITIAL>{DIGIT}+ { return cf.newSymbol(\"\", sym._INTEGER_, left_loc(), right_loc(), Integer.valueOf(yytext())); }"
    , ifC catIdent
        "<YYINITIAL>{LETTER}{IDENT}* { return cf.newSymbol(\"\", sym._IDENT_, left_loc(), right_loc(), yytext().intern()); }"
    , "<YYINITIAL>[ \\t\\r\\n\\f] { /* ignore white space. */ }"
    , if jflex == JFlexCup
        then "<<EOF>> { return cf.newSymbol(\"EOF\", sym.EOF, left_loc(), left_loc()); }"
        else ""
    , if rp == RecordPositions
        then ". { throw new Error(\"Illegal Character <\"+yytext()+\"> at \"+(yyline+1)" <>
          (if jflex == JFlexCup then "+\":\"+(yycolumn+1)+\"(\"+yychar+\")\"" else "") <> "); }"
        else ". { throw new Error(\"Illegal Character <\"+yytext()+\">\"); }"
    ]
  where
    ifC :: TokenCat -> Doc -> Doc
    ifC cat s = if isUsedCat cf (TokenCat cat) then s else ""
    userDefTokens = vcat
        [ "<YYINITIAL>" <> text (printRegJLex jflex exp)
            <+> "{ return cf.newSymbol(\"\", sym." <> text name
            <> ", left_loc(), right_loc(), yytext().intern()); }"
        | (name, exp) <- tokenPragmas cf ]
    strStates = vcat --These handle escaped characters in Strings.
        [ "<YYINITIAL>\"\\\"\" { left = left_loc(); yybegin(STRING); }"
        , "<STRING>\\\\ { yybegin(ESCAPED); }"
        , "<STRING>\\\" { String foo = pstring; pstring = new String(); yybegin(YYINITIAL); return cf.newSymbol(\"\", sym._STRING_, left, right_loc(), foo.intern()); }"
        , "<STRING>.  { pstring += yytext(); }"
        , "<STRING>\\r\\n|\\r|\\n { throw new Error(\"Unterminated string on line \" + left.getLine() " <>
          (if jflex == JFlexCup then "+ \" begining at column \" + left.getColumn()" else "") <> "); }"
        , if jflex == JFlexCup
          then "<STRING><<EOF>> { throw new Error(\"Unterminated string at EOF, beginning at \" + left.getLine() + \":\" + left.getColumn()); }"
          else ""
        , "<ESCAPED>n { pstring +=  \"\\n\"; yybegin(STRING); }"
        , "<ESCAPED>t  { pstring += \"\\t\"; yybegin(STRING); }"
        , "<ESCAPED>r  { pstring += \"\\r\"; yybegin(STRING); }"
        , "<ESCAPED>f  { pstring += \"\\f\"; yybegin(STRING); }"
        , "<ESCAPED>\\\" { pstring += \"\\\"\"; yybegin(STRING); }"
        , "<ESCAPED>\\\\ { pstring += \"\\\\\"; yybegin(STRING); }"
        , "<ESCAPED>.  { pstring += yytext(); yybegin(STRING); }"
        , "<ESCAPED>\\r\\n|\\r|\\n { throw new Error(\"Unterminated string on line \" + left.getLine() " <>
          (if jflex == JFlexCup then "+ \" beginning at column \" + left.getColumn()" else "") <> "); }"
        , if jflex == JFlexCup
          then "<ESCAPED><<EOF>> { throw new Error(\"Unterminated string at EOF, beginning at \" + left.getLine() + \":\" + left.getColumn()); }"
          else ""
        ]
    chStates = vcat --These handle escaped characters in Chars.
        [ "<YYINITIAL>\"'\" { left = left_loc(); yybegin(CHAR); }"
        , "<CHAR>\\\\ { yybegin(CHARESC); }"
        , "<CHAR>[^'] { yybegin(CHAREND); return cf.newSymbol(\"\", sym._CHAR_, left, right_loc(), Character.valueOf(yytext().charAt(0))); }"
        , "<CHAR>\\r\\n|\\r|\\n { throw new Error(\"Unterminated character literal on line \" + left.getLine() " <>
          (if jflex == JFlexCup then "+ \" beginning at column \" + left.getColumn()" else "") <> "); }"
        , if jflex == JFlexCup
          then "<CHAR><<EOF>> { throw new Error(\"Unterminated character literal at EOF, beginning at \" + left.getLine() + \":\" + left.getColumn()); }"
          else ""
        , "<CHARESC>n { yybegin(CHAREND); return cf.newSymbol(\"\", sym._CHAR_, left, right_loc(), Character.valueOf('\\n')); }"
        , "<CHARESC>t { yybegin(CHAREND); return cf.newSymbol(\"\", sym._CHAR_, left, right_loc(), Character.valueOf('\\t')); }"
        , "<CHARESC>r { yybegin(CHAREND); return cf.newSymbol(\"\", sym._CHAR_, left, right_loc(), Character.valueOf('\\r')); }"
        , "<CHARESC>f { yybegin(CHAREND); return cf.newSymbol(\"\", sym._CHAR_, left, right_loc(), Character.valueOf('\\f')); }"
        , "<CHARESC>. { yybegin(CHAREND); return cf.newSymbol(\"\", sym._CHAR_, left, right_loc(), Character.valueOf(yytext().charAt(0))); }"
        , "<CHARESC>\\r\\n|\\r|\\n { throw new Error(\"Unterminated character literal on line \" + left.getLine() " <>
          (if jflex == JFlexCup then "+ \" beginning at column \" + left.getColumn()" else "") <> "); }"
        , if jflex == JFlexCup
          then "<CHARESC><<EOF>> { throw new Error(\"Unterminated character literal at EOF, beginning at \" + left.getLine() + \":\" + left.getColumn()); }"
          else ""
        , "<CHAREND>\"'\" {yybegin(YYINITIAL);}"
        , "<CHAREND>\\r\\n|\\r|\\n { throw new Error(\"Unterminated character literal on line \" + left.getLine() " <>
          (if jflex == JFlexCup then "+ \" beginning at column \" + left.getColumn()" else "") <> "); }"
        , if jflex == JFlexCup
          then "<CHAREND><<EOF>> { throw new Error(\"Unterminated character literal at EOF, beginning at \" + left.getLine() + \":\" + left.getColumn()); }"
          else ""
        ]

lexComments :: ([(String, String)], [String]) -> Doc
lexComments (m,s) = vcat $ concat
  [ map lexSingleComment s
  , zipWith lexMultiComment m commentStates
  ]

-- | Create lexer rule for single-line comments.
--
-- >>> lexSingleComment "--"
-- <YYINITIAL>"--"[^\n]* { /* skip */ }
--
-- >>> lexSingleComment "\""
-- <YYINITIAL>"\""[^\n]* { /* skip */ }
lexSingleComment :: String -> Doc
lexSingleComment c =
  "<YYINITIAL>" <> cstring c <>  "[^\\n]* { /* skip */ }"

-- | Create lexer rule for multi-lines comments.
--
-- There might be a possible bug here if a language includes 2 multi-line
-- comments. They could possibly start a comment with one character and end it
-- with another. However this seems rare.
--
-- >>> lexMultiComment ("{-", "-}") "COMMENT"
-- <YYINITIAL>"{-" { yybegin(COMMENT); }
-- <COMMENT>"-}" { yybegin(YYINITIAL); }
-- <COMMENT>. { /* skip */ }
-- <COMMENT>[\n] { /* skip */ }
--
-- >>> lexMultiComment ("\"'", "'\"") "COMMENT"
-- <YYINITIAL>"\"'" { yybegin(COMMENT); }
-- <COMMENT>"'\"" { yybegin(YYINITIAL); }
-- <COMMENT>. { /* skip */ }
-- <COMMENT>[\n] { /* skip */ }
--
lexMultiComment :: (String, String) -> String -> Doc
lexMultiComment (b,e) comment = vcat
    [ "<YYINITIAL>" <> cstring b <+> "{ yybegin(" <> text comment <> "); }"
    , commentTag <> cstring e <+> "{ yybegin(YYINITIAL); }"
    , commentTag <> ". { /* skip */ }"
    , commentTag <> "[\\n] { /* skip */ }"
    ]
  where
  commentTag = text $ "<" ++ comment ++ ">"
