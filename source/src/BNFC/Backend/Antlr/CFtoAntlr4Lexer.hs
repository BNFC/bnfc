{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Java Antlr4 Lexer generator
    Copyright (C) 2015  Author:  Gabriele Paganelli

    Description   : This module generates the Antlr4 input file.
                    Based on CFtoJLex15.hs

    Author        : Gabriele Paganelli (gapag@distruzione.org)
    Created       : 15 Oct, 2015

-}

module BNFC.Backend.Antlr.CFtoAntlr4Lexer ( cf2AntlrLex ) where

import Prelude hiding ((<>))

import Text.PrettyPrint
import BNFC.CF
import BNFC.Backend.Antlr.RegToAntlrLexer
import BNFC.Backend.Common.NamedVariables

-- | Creates a lexer grammar.
-- Since antlr token identifiers must start with an uppercase symbol,
-- I prepend "Surrogate_id_SYMB_" to the identifier.
-- This introduces risks of clashes if somebody uses the same identifier for
-- user defined tokens. This is not handled.
-- returns the environment because the parser uses it.
cf2AntlrLex :: String -> CF -> (Doc, KeywordEnv)
cf2AntlrLex lang cf = (,env) $ vcat
    [ prelude lang
    , cMacros
    -- unnamed symbols (those in quotes, not in token definitions)
    , lexSymbols env
    , restOfLexerGrammar cf
    ]
  where
    env = zip (cfgSymbols cf ++ reservedWords cf) $ map (("Surrogate_id_SYMB_" ++) . show) [0 :: Int ..]


-- | File prelude
prelude :: String -> Doc
prelude lang = vcat
    [ "// Lexer definition for use with Antlr4"
    , "lexer grammar" <+> text lang <> "Lexer;"
    ]

--For now all categories are included.
--Optimally only the ones that are used should be generated.
cMacros :: Doc
cMacros = vcat
    [ "// Predefined regular expressions in BNFC"
    , frg "LETTER  : CAPITAL | SMALL"
    , frg "CAPITAL : [A-Z\\u00C0-\\u00D6\\u00D8-\\u00DE]"
    , frg "SMALL   : [a-z\\u00DF-\\u00F6\\u00F8-\\u00FF]"
    , frg "DIGIT   : [0-9]"
    ]
  where frg a = "fragment" <+> a <+> ";"

escapeChars :: String -> String
escapeChars = concatMap escapeCharInSingleQuotes

-- |
-- >>> lexSymbols [("foo","bar")]
-- bar : 'foo' ;
-- >>> lexSymbols [("\\","bar")]
-- bar : '\\' ;
-- >>> lexSymbols [("/","bar")]
-- bar : '/' ;
-- >>> lexSymbols [("~","bar")]
-- bar : '~' ;
lexSymbols :: KeywordEnv -> Doc
lexSymbols ss = vcat $  map transSym ss
  where
    transSym (s,r) = text r <>  " : '" <> text (escapeChars s) <> "' ;"

-- | Writes rules for user defined tokens, and, if used, the predefined BNFC tokens.
restOfLexerGrammar :: CF -> Doc
restOfLexerGrammar cf = vcat
    [ lexComments (comments cf)
    , ""
    , userDefTokens
    , ifString strdec
    , ifChar chardec
    , ifC catDouble [
        "// Double predefined token type",
        "DOUBLE : DIGIT+ '.' DIGIT+ ('e' '-'? DIGIT+)?;"
        ]
    , ifC catInteger [
        "//Integer predefined token type",
        "INTEGER : DIGIT+;"
        ]
    , ifC catIdent [
        "// Identifier token type" ,
        "fragment" ,
        "IDENTIFIER_FIRST : LETTER | '_';",
        "IDENT : IDENTIFIER_FIRST (IDENTIFIER_FIRST | DIGIT)*;"
        ]
    , "// Whitespace"
    , "WS : (' ' | '\\r' | '\\t' | '\\n' | '\\f')+ ->  skip;"
    , "// Escapable sequences"
    , "fragment"
    , "Escapable : ('\"' | '\\\\' | 'n' | 't' | 'r' | 'f');"
    , "ErrorToken : . ;"
    , ifString stringmodes
    , ifChar charmodes
    ]
  where
    ifC cat s     = if isUsedCat cf (TokenCat cat) then vcat s else ""
    ifString      = ifC catString
    ifChar        = ifC catChar
    strdec        = [ "// String token type"
                    , "STRING : '\"' -> more, mode(STRINGMODE);"
                    ]
    chardec       = ["CHAR : '\\''   -> more, mode(CHARMODE);"]
    userDefTokens = vcat
        [ text name <> " : " <> text (printRegJLex exp) <> ";"
        | (name, exp) <- tokenPragmas cf ]
    stringmodes   = [ "mode STRESCAPE;"
        , "STRESCAPED : Escapable  -> more, popMode ;"
        , "mode STRINGMODE;"
        , "STRINGESC : '\\\\' -> more , pushMode(STRESCAPE);"
        , "STRINGEND : '\"' ->  type(STRING), mode(DEFAULT_MODE);"
        , "STRINGTEXT : ~[\"\\\\] -> more;"
        ]
    charmodes     = [ "mode CHARMODE;"
        , "CHARANY     :  ~['\\\\] -> more, mode(CHAREND);"
        , "CHARESC     :  '\\\\'  -> more, pushMode(CHAREND),pushMode(ESCAPE);"
        , "mode ESCAPE;"
        , "ESCAPED : (Escapable | '\\'')  -> more, popMode ;"
        , "mode CHAREND;"
        , "CHARENDC     :  '\\''  -> type(CHAR), mode(DEFAULT_MODE);"
        ]

lexComments :: ([(String, String)], [String]) -> Doc
lexComments ([],[]) = ""
lexComments (m,s) = vcat
    (prod "COMMENT_antlr_builtin" lexSingleComment s ++
         prod "MULTICOMMENT_antlr_builtin" lexMultiComment m )

  where
    prod bg lc ty = [bg, ": ("] ++ punctuate "|" (map lc ty) ++ skiplex
    skiplex       = [") -> skip;"]

-- | Create lexer rule for single-line comments.
--
-- >>> lexSingleComment "--"
-- '--' ~[\r\n]* (('\r'? '\n')|EOF)
--
-- >>> lexSingleComment "\""
-- '"' ~[\r\n]* (('\r'? '\n')|EOF)
lexSingleComment :: String -> Doc
lexSingleComment c =
    "'" <>text (escapeChars c) <>  "' ~[\\r\\n]* (('\\r'? '\\n')|EOF)"

-- | Create lexer rule for multi-lines comments.
--
-- There might be a possible bug here if a language includes 2 multi-line
-- comments. They could possibly start a comment with one character and end it
-- with another. However this seems rare.
--
-- >>> lexMultiComment ("{-", "-}")
-- '{-' (.)*? '-}'
--
-- >>> lexMultiComment ("\"'", "'\"")
-- '"\'' (.)*? '\'"'
lexMultiComment :: (String, String) -> Doc
lexMultiComment (b,e) =
         "'" <> text (escapeChars b)
        <>"' (.)*? '"<> text (escapeChars e)
        <> "'"
