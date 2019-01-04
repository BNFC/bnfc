module BNFC.Backend.Javascript.CFtoJSLex (cf2jsLex) where

import Data.List
import Text.Printf
import Data.Maybe

import BNFC.CF
import BNFC.Backend.Common.NamedVariables

import BNFC.Backend.Javascript.RegToJSLex

cf2jsLex :: CF -> (String, SymEnv)
cf2jsLex cf = (unlines [
        macros
        , jsLex $ (userComments cf)
                  ++ (userSymbols env)
                  ++ (userTokens env' cf)
                  ++ defaultTokens cf
    ], env')
    where
       env = makeSymEnv tokens (0 :: Int)
       env' = env ++ (makeSymEnv pragmas (length env))
       pragmas = map show $ fst (unzip (tokenPragmas cf))
       makeSymEnv [] _ = []
       makeSymEnv (s:symbs) n = (s, "SYMB" ++ (show n)) : (makeSymEnv symbs (n+1))
       -- The longest tokens first
       tokens = sortBy (\a b -> compare (length b) (length a)) (map fst $ cfTokens cf)

macros :: String
macros = unlines [
      "letter [a-zA-Z]"
      , "capital [A-Z]"
      , "small [a-z]"
      , "digit [0-9]"
      , "ident [a-zA-Z0-9\"_\"]"
    ]

jsLex :: [(Int, String -> String)] -> String
jsLex s = unlines [
      "/* Lexical grammar generated by BNFC */"
      , "%%"
      , concatMap (\(i, f) -> f $ spaces (maxDef - i)) s
    ]
    where maxDef = 1 + (maximum $ map fst s)
          spaces i = replicate i ' '

userComments :: CF -> [(Int, String -> String)]
userComments cf = (map (indent . single) s) ++ (map (indent . multi) m)
    where (m, s) = comments cf
          single a = printf "\"%s\"[^\\n]*" a
          multi (a, b) = printf "\"%s\"(.|\\n|\\r)*?\"%s\"" a b
          indent a = (length a, \t -> printf (a ++ "%s/* comment */\n") t)

userSymbols :: SymEnv -> [(Int, String -> String)]
userSymbols se = map userSymbol se
    where
        userSymbol (s, r) = let e = quote s in (length e, \t -> printf "%s%sreturn '%s'\n" e t r)
        quote s = "\"" ++ (concatMap escape s) ++ "\""
        escape c = if c == '\\' then '\\':[c] else [c]

userTokens :: SymEnv -> CF -> [(Int, String -> String)]
userTokens se cf = map userToken $ tokenPragmas cf
    where userToken (name, exp) = let s = (printRegJSLex exp) in
                                   (length s, \t -> printf "%s%sreturn '%s'\n"
                                                     s t (symb $ show name))
          symb name = fromJust $ lookup name se

defaultTokens :: CF -> [(Int, String -> String)]
defaultTokens cf = special ++ [
        (3, \t -> printf "\\s+%s/* skip whitespace */\n" t)
        , (7, \t -> printf "<<EOF>>%sreturn 'EOF'\n" t)
        , (1, \t -> printf ".%sreturn 'INVALID'\n" t)
    ]
    where special =
            ifC "Double" "{digit}+\".\"{digit}+" "return 'DOUBLE'"
            ++ ifC "Ident" "{letter}{ident}*" "return 'IDENT'"
            ++ ifC "Integer" "{digit}+" "return 'INTEGER'"
            ++ ifC "Char" "\\'(?:[^'\\\\]|\\\\.)*\\'" "yytext = yytext.substr(1,yyleng-2); return 'CHAR'"
            ++ ifC "String" "\\\"(?:[^\"\\\\]|\\\\.)*\\\"" "yytext = yytext.substr(1,yyleng-2); return 'STRING'"
          ifC cat s r = if isUsedCat cf (TokenCat cat) then [(length s, \t -> printf (s++"%s"++r++"\n") t)] else []
