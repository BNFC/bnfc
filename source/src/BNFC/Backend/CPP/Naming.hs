module BNFC.Backend.CPP.Naming where

import BNFC.Utils

cReservedWords, cppReservedWords :: [String]
cReservedWords = [ "auto", "const", "double", "float", "int", "short", "struct"
    , "unsigned", "break", "continue", "else", "for", "long", "signed"
    , "switch", "void", "case", "default", "enum", "goto", "register", "sizeof"
    , "typedef", "volatile", "char", "do", "extern", "if", "return", "static"
    , "union", "while" ]

cppReservedWords = cReservedWords ++ [ "asm", "dynamic_cast", "namespace"
    , "reinterpret_cast" , "try", "bool", "explicit", "new", "static_cast"
    , "typeid", "catch" , "false", "operator", "template", "typename", "class"
    , "friend" , "private", "this", "using", "const_cast", "inline", "public"
    , "throw" , "virtual", "delete", "mutable", "protected", "true", "wchar_t"
    , "and", "bitand", "compl", "not_eq", "or_eq", "xor_eq", "and_eq", "bitor"
    , "not", "or", "xor" ]

mkVariable :: String -> String
mkVariable = mkName cppReservedWords SnakeCase
