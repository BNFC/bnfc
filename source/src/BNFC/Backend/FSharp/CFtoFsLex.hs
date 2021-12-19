{-
    BNF Converter: fslex Generator
    Copyright (C) 2021  Author:  Grzegorz Dziadkiewicz

-}


-- based on BNFC OCaml backend

module BNFC.Backend.FSharp.CFtoFsLex (cf2fslex) where

import Prelude hiding ((<>))

import Data.Char (ord)
import qualified Data.List as List
import Text.PrettyPrint hiding (render)
import qualified Text.PrettyPrint as PP

import BNFC.Abs
import BNFC.CF
import BNFC.Backend.Common (asciiKeywords, unicodeAndSymbols)
import BNFC.Backend.FSharp.CFtoFsYacc (terminal)
import BNFC.Backend.FSharp.FSharpUtil (mkEsc, fsharpTokenName, indent)
import BNFC.Lexing (mkRegMultilineComment)
import BNFC.Utils (cstring, unless, (+++))

cf2fslex :: String -> String -> CF -> String
cf2fslex lexMod parserMod cf = unlines $ List.intercalate [""]
  [ header lexMod parserMod cf
  , cMacros
  , rMacros cf
  , uMacros cf
  , [ PP.render $ rules cf ]
  ]

header :: String -> String -> CF -> [String]
header lexerMod parserMod cf = List.intercalate [""] . filter (not . null) $ concat
  [ [ [ "(* Lexer definition for fslex. *)"
      , "{"
      , "module " ++ lexerMod
      , "open " ++ parserMod
      , "open System"
      , "open System.Collections.Generic"
      , "open FSharp.Text.Lexing"
      , ""
      ]
    ]
  , hashtables cf
  , [ [ "let unescapeInitTail (s:string) : string ="
      , "  let rec unesc s ="
      , "    match s with"
      , "    | '\\\\'::c::cs when List.contains c ['\\\"'; '\\\\'; '\\\''] -> c :: unesc cs"
      , "    | '\\\\'::'n'::cs  -> '\\n' :: unesc cs"
      , "    | '\\\\'::'t'::cs  -> '\\t' :: unesc cs"
      , "    | '\\\"'::[]    -> []"
      , "    | c::cs      -> c :: unesc cs"
      , "    | _         -> []"
      , "  s.ToCharArray() |> List.ofArray |> List.tail |> unesc |> Array.ofList |> String"
      , ""
      , "let incr_lineno (lexbuf:LexBuffer<_>) : unit ="
      , "    lexbuf.EndPos <- lexbuf.EndPos.NextLine"
      , "let lexeme (lexbuf:LexBuffer<_>) : string ="
      , "    LexBuffer<_>.LexemeString lexbuf"
      , "}"
      ]
    ]
  ]

-- | Set up hashtables for reserved symbols and words.
hashtables :: CF -> [[String]]
hashtables cf =
  [ ht "symbol_table"  $ unicodeAndSymbols cf
  , ht "resword_table" $ asciiKeywords cf
  ]
  where
    ht :: String -> [String] -> [String]
    ht table syms = unless (null syms) $
        [ "let" +++ table +++ "="
        , indent 1 "Map.ofList"
        , indent 2 "["
        , unlines $ map (indent 3) keyvals
        , indent 2 "]"
        ]
      where
        keyvals = map (\ s -> concat [ "(", mkEsc s, ", ", terminal cf s, ")" ]) syms

cMacros :: [String]
cMacros =
  [ "(* BNFC character classes *)"
  , "let letter = ['a'-'z' 'A'-'Z' '\\192'-'\\214' '\\216'-'\\246' '\\248'-'\\255'] (*  isolatin1 letter FIXME *)"
  , "let upper  = ['A'-'Z' '\\192'-'\\214' '\\216'-'\\221']  (*  capital isolatin1 letter FIXME *)"
  , "let lower  = ['a'-'z' '\\222'-'\\246' '\\248'-'\\255'] (*  small isolatin1 letter FIXME *)"
  , "let digit  = ['0'-'9']                             (*  digit *)"
  , "let idchar = letter | digit | ['_' '\\'']         (*  identifier character *)"
  , "let universal = _                                  (* universal: any character *)"
  ]

rMacros :: CF -> [String]
rMacros cf
  | null symbs = []
  | otherwise  =
      [ "(* reserved words consisting of special symbols *)"
      , unwords $ "let rsyms =" : List.intersperse "|" (map mkEsc symbs)
      ]
  where symbs = unicodeAndSymbols cf

-- user macros, derived from the user-defined tokens
uMacros :: CF -> [String]
uMacros cf = if null res then [] else "(* user-defined token types *)" : res
  where res = ["let " ++ name ++ " = " ++ rep | (name, rep, _, _) <- userTokens cf]

-- | Returns the tuple of @(reg_name, reg_representation, token_name, is_position_token)@.

userTokens :: CF -> [(String, String, String, Bool)]
userTokens cf =
  [ (fsharpTokenName name, printRegFSharp reg, name, pos)
  | TokenReg n pos reg <- cfgPragmas cf
  , let name = wpThing n
  ]

-- | Make FsLex rule
-- >>> mkRule "token" [("REGEX1","ACTION1"),("REGULAREXPRESSION2","ACTION2"),("...","...")]
-- (* lexing rules *)
-- rule token =
--   parse REGEX1  { ACTION1 }
--       | REGULAREXPRESSION2
--                 { ACTION2 }
--       | ...     { ... }
--
-- If no regex are given, we dont create a lexer rule:
-- >>> mkRule "empty" []
-- <BLANKLINE>
mkRule :: Doc -> [(Doc,Doc)] -> Doc
mkRule _ [] = empty
mkRule entrypoint (r:rs) = vcat
    [ "(* lexing rules *)"
    , "rule" <+> entrypoint <+> "="
    , nest 2 $ hang "parse" 4 $ vcat $
        nest 2 (mkOne r) : map (("|" <+>) . mkOne) rs
    ]
  where
    mkOne (regex, action) = regex $$ nest 8 (hsep ["{", action, "}"])

-- | Create regex for single line comments
-- >>> mkRegexSingleLineComment "--"
-- "--" [^'\n']*
-- >>> mkRegexSingleLineComment "\""
-- "\"" [^'\n']*
mkRegexSingleLineComment :: String -> Doc
mkRegexSingleLineComment s = cstring s <+> "[^'\\n']*"

-- | Create regex for multiline comments.
-- >>> mkRegexMultilineComment "<!--" "-->"
-- "<!--" [^ '-']* '-' ([^ '-']+ '-')* '-' ([^ '-' '>'][^ '-']* '-' ([^ '-']+ '-')* '-' | '-')* '>'
--
-- >>> mkRegexMultilineComment "\"'" "'\""
-- "\"'" [^ '\'']* '\'' ([^ '"' '\''][^ '\'']* '\'' | '\'')* '"'
mkRegexMultilineComment :: String -> String -> Doc
mkRegexMultilineComment b e = text $ printRegFSharp $ mkRegMultilineComment b e

-- | Uses the function from above to make a lexer rule from the CF grammar
rules :: CF -> Doc
rules cf = mkRule "token" $
    -- comments
    [ (mkRegexSingleLineComment s, "token lexbuf") | s <- singleLineC ]
    ++
    [ (mkRegexMultilineComment b e, "token lexbuf") | (b,e) <- multilineC]
    ++
    -- reserved keywords
    [ ( "rsyms"
      , vcat 
        [ "let x = lexeme lexbuf"
        , "match Map.tryFind x symbol_table with"
        , "| Some result -> result"
        , "| None -> failwith (\"internal lexer error: reserved symbol \" + x + \" not found in hashtable\")"
        ] )
      | not (null (cfgSymbols cf))]
    ++
    -- user tokens
    [ (text n , tokenAction pos (text t)) | (n,_,t,pos) <- userTokens cf]
    ++
    -- predefined tokens
    [ ( "letter idchar*", tokenAction False "Ident" ) ]
    ++
    -- integers
    [ ( "digit+", "lexbuf |> lexeme |> int |> TOK_Integer" )
    -- doubles
    , ( "digit+ '.' digit+ ('e' ('-')? digit+)?"
      , "lexbuf |> lexeme |> float |> TOK_Double" )
    -- strings
    , ( "'\\\"' (([^ '\\\"' '\\\\' '\\n']) | ('\\\\' ('\\\"' | '\\\\' | '\\\'' | 'n' | 't' | 'r')))* '\\\"'"
      , "lexbuf |> lexeme |> unescapeInitTail |> TOK_String" )
    -- chars
    , ( "'\\'' (([^ '\\\'' '\\\\']) | ('\\\\' ('\\\\' | '\\\'' | 'n' | 't' | 'r'))) '\\\''"
      , "TOK_Char (lexeme lexbuf).[1]")
    -- spaces
    , ( "[' ' '\\t' '\\r']", "token lexbuf")
    -- new lines
    , ( "'\\n' | \"\\r\\n\"", "incr_lineno lexbuf; token lexbuf" )
    -- end of file
    , ( "eof", "TOK_EOF" )
    ]
  where
    (multilineC, singleLineC) = comments cf
    tokenAction pos t = case asciiKeywords cf of
        [] -> "TOK_" <> t <+> arg "(lexeme lexbuf)"
        _  -> vcat 
                [ "let l = lexeme lexbuf"
                , "Map.tryFind l resword_table"
                , "|> Option.defaultValue (TOK_" <> t <+> arg "l)"
                ]
      where
      arg l | pos       = text $ "((lexeme_start lexbuf, lexeme_end lexbuf), " ++ l ++ ")"
            | otherwise = text l

-------------------------------------------------------------------
-- Modified from the inlined version of former @RegToAlex@.
-------------------------------------------------------------------

-- modified from pretty-printer generated by the BNF converter

-- the top-level printing method
printRegFSharp :: Reg -> String
printRegFSharp = render . prt 0

-- you may want to change render and parenth

render :: [String] -> String
render = rend 0
    where rend :: Int -> [String] -> String
          rend i ss = case ss of
                        "["      :ts -> cons "["  $ rend i ts
                        "("      :ts -> cons "("  $ rend i ts
                        t  : "," :ts -> cons t    $ space "," $ rend i ts
                        t  : ")" :ts -> cons t    $ cons ")"  $ rend i ts
                        t  : "]" :ts -> cons t    $ cons "]"  $ rend i ts
                        t        :ts -> space t   $ rend i ts
                        _            -> ""

          cons s t  = s ++ t
          space t s = if null s then t else t ++ " " ++ s

parenth :: [String] -> [String]
parenth ss = ["("] ++ ss ++ [")"]

-- the printer class does the job
class Print a where
  prt :: Int -> a -> [String]
  prtList :: [a] -> [String]
  prtList = concat . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ c   = [charLiteral c]
  prtList s = [show s] -- map (concat . prt 0) s

charLiteral :: Char -> String
charLiteral c
  | ord c <= 256 = show c
  | otherwise    = ['"', c, '"']  -- ocamllex does not accept unicode character literals TODO: Check if it's true for fslex

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if j<i then parenth else id

instance Print Identifier where
  prt _ (Identifier (_, i)) = [i]

instance Print Reg where
  prt i e = case e of
   RSeq reg0 reg   -> prPrec i 2 (concat [prt 2 reg0 , prt 3 reg])
   RAlt reg0 reg   -> prPrec i 1 (concat [prt 1 reg0 , ["|"] , prt 2 reg])
   RMinus RAny (RChar c) -> ["[^", charLiteral c, "]"]
   RMinus RAny (RAlts str) -> concat [ ["[^"], map charLiteral str, ["]"] ]
   RMinus reg0 reg -> prPrec i 1 (concat [prt 2 reg0 , ["#"] , prt 2 reg])
   RStar reg       -> concat [prt 3 reg , ["*"]]
   RPlus reg       -> concat [prt 3 reg , ["+"]]
   ROpt reg        -> concat [prt 3 reg , ["?"]]
   REps            -> ["\"\""]
   RChar c         -> [ charLiteral c ]
   -- TODO: Check if it's also the case for fslex
   -- ocamllex accepts unicode characters only in string literals.
   -- Thus we translate RAlts to a disjunction rather than a character set
   RAlts str       -> prPrec i 1 $ List.intersperse "|" $ map charLiteral str
   -- RAlts str       -> concat [ ["["], map charLiteral str, ["]"] ]
   RSeqs str       -> [ show str ]
   RDigit          -> ["digit"]
   RLetter         -> ["letter"]
   RUpper          -> ["upper"]
   RLower          -> ["lower"]
   RAny            -> ["universal"]
