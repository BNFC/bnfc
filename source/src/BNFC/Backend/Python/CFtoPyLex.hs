
{-  
    BNF Converter: Python lexer generator
    Copyright (C) 2024  Author: Bjorn Werner
-}

module BNFC.Backend.Python.CFtoPyLex ( cf2PyLex ) where

import BNFC.CF

import BNFC.Backend.Python.RegToFlex (printRegFlex, escapeChar)
import BNFC.Backend.Python.PyHelpers


-- | The entrypoint, returns LexTokens.py and the unicode mapping.
cf2PyLex :: CF -> (String, [(String, String)])
cf2PyLex cf = (, tokensPly) $ unlines 
  [ "import ply.lex as lex\n"
  , ""
  , createReservedMap reservedWordsEnv
  , "# PLY tokens:\n" ++ plyTokens ++ "\n"
  , "# PLY tokens with RegEx:"
  , unlines plyTokensRegEx
  , "# Literals:"
  , plyLiterals cf
  , "# Comments:"
  , unlines singleComments
  , unlines multiComments
  , footer
  ]
  where
    -- The reserved keywords and the symbols are zipped with a
    -- unicode representation, which are needed for the parsing.

    -- Reserved keywords -> [("int", "R_...")]
    reservedWordsVar :: [String]
    reservedWordsVar = reservedWords cf

    reservedWordsEnv :: [(String, String)]
    reservedWordsEnv = 
      zip reservedWordsVar (map (("R" ++) . toOrd) reservedWordsVar)

    -- Symbols -> [("+", "S_43")]
    literalsVar :: [String]
    literalsVar = literals cf

    strOps :: [String]
    strOps = map fst (cfTokens cf)

    strOpsFiltered = filterOut strOps reservedWordsVar
    strOpsFilteredSymbols = map (("S" ++) . toOrd) strOpsFiltered
    
    strOpsAndSymbols :: [(String, String)]
    strOpsAndSymbols = zip strOpsFiltered strOpsFilteredSymbols

    presentSymbols :: [String]
    presentSymbols = 
      map addCitationSigns (strOpsFilteredSymbols ++ literalsVar)

    -- Defining the variables for the lexer.
    plyTokens =
      "tokens = reserved + (" ++ concat (map (++ ",") presentSymbols) ++ ")"
    plyTokensRegEx = map createRegEx strOpsAndSymbols
    
    tokensPly :: [(String, String)]
    tokensPly = reservedWordsEnv ++ strOpsAndSymbols 
    
    -- Comments
    (multiMatchers, singleMatchers) = comments cf
    singleComments = map createLineCommentMatcher singleMatchers
    multiComments = map createMultiLineCommentMatcher multiMatchers


-- | Creates tokens for the lexer, such as "t_S_43 = r'\+'".
createRegEx :: (String, String) -> String
createRegEx (s, u) = "t_" ++ u ++ " = r'" ++ concat (map escapeChar s) ++ "'"


-- | For single-line comments
createLineCommentMatcher :: String -> String
createLineCommentMatcher r = unlines
  [ "def t_C" ++ (toOrd r) ++ "(t):"
  , "\tr'" ++ concat (map escapeChar r) ++ ".*'"
  , "\tpass"
  ]


-- | For multi-line comments
createMultiLineCommentMatcher :: (String, String) -> String
createMultiLineCommentMatcher (s, e) = unlines
  [ "def t_C" ++ (toOrd (s ++ e)) ++ "(t):"
  , "\tr'" ++ (escaped s) ++ "([\\s\\S]*?)" ++ (escaped e) ++ "'"
  , "\tpass"
  ]
  where
    escaped s = concat $ map escapeChar s


-- | The reserved_map contains mappings for reserved keywords,
--   such as 'int' : 'R_105_110_116'.
createReservedMap :: [(String, String)] -> String
createReservedMap xs = unlines 
  [ "reserved_map = {"
  , unlines rows
  , "}"
  , ""
  , "reserved = ("
  , unlines rowsSnd
  , ")"
  ]
  where
    rows :: [String]
    rows = ["\t'" ++ w ++ "' : '" ++ u ++ "'," | (w, u) <- xs]

    rowsSnd = ["\t'" ++ u ++ "'," | (_, u) <- xs]


-- | Creates lexer definitions for the lexer which are interpreted using
--   the inspect module to retrieve useful information, for example:
--   def t_String(t):
--     r'"[^"]+"'
--     t.type = reserved_map.get(t.value, ’String’)
--     return t
plyLiterals :: CF -> String
plyLiterals cf = unlines $ concat
  [ 
  ifC catString  [createLexFunc "String" "\"(\\\\\"|[^\"])*\""]
  , ifC catChar    
    [createLexFunc "Char" "\\'(\\\\x[0-9a-f][0-9a-f]|\\\\?[\\S\\s])\\'"]
  , ifC catDouble  [createLexFunc "Double" "\\d+\\.\\d+(e-?\\d+)?"]
  , ifC catInteger [createLexFunc "Integer" "\\d+"]
  -- Prolog requires user defined tokens to have priority over Ident; C 
  -- requires Double to have priority over user defined tokens, as C has
  -- "CDouble" matching "3." in 3.14. The lexer definitions rely on the order
  -- for priority, not the length.
  , userDefTokens 
  , ifC catIdent   [createLexFunc "Ident" "[A-Za-z]\\w*"]
  -- If there is no Ident present, we need a lexer definition for reserved
  -- words:
  , if not (isUsedCat cf (TokenCat catIdent)) && length (reservedWords cf) > 0
    then [createLexFunc "" "[A-Za-z]\\w*"]
    else []
  ]
  where
  ifC :: TokenCat -> [String] -> [String]
  ifC cat s = if isUsedCat cf (TokenCat cat) then s else []

  userDefTokens :: [String] 
  userDefTokens = [
    createLexFunc name (printRegFlex exp) | (name, exp) <- tokenPragmas cf
    ]


-- | Creates a Lexing definition for a Literal
--   If no Literal name is used, this is just a reserved_map lookup.
createLexFunc :: String -> String -> String
createLexFunc name regex = unlines 
  [ "def t_" ++ (if name /= "" then name else "_NoIdentPresent") ++ "(t):"
  , "\tr'" ++ regex ++ "'"
  , if name /= ""
    then "\tt.type = reserved_map.get(t.value, '" ++ name ++ "')"
    else "\tt.type = reserved_map.get(t.value)"
  , "\treturn t"
  ]


-- | Adds lexer definitions to ignore whitespaces, and a testing block
--   which attempts tokenize some input, like:  python3 LexTokens.py < input
footer :: String
footer = unlines 
  [ "# Ignored characters:"
  , "t_ignore = ' \\t'"
  , ""
  , "# Ignored token with an action associated with it:"
  , "def t_ignore_newline(t):"
  , "\tr'\\n+'"
  , "\tt.lexer.lineno += t.value.count('\\n')"
  , ""
  , "# Error handler for illegal characters:"
  , "def t_error(t):"
  , "\tprint('Illegal character', 'line', str(t.lineno) + ':', t.value[0], 'ascii:', ord(t.value[0]))"
  , "\tquit()"
  , ""
  , "if __name__ == \"__main__\":"
  , "\tlexer = lex.lex()"
  , "\tlex.runmain(lexer)"
  ]


