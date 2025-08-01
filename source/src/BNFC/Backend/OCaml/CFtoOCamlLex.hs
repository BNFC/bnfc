{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: ocamllex Generator
    Copyright (C) 2005  Author:  Kristofer Johannisson

-}


-- based on BNFC Haskell backend

module BNFC.Backend.OCaml.CFtoOCamlLex (cf2ocamllex) where

import Prelude hiding ((<>))

import Data.Char (ord)
import qualified Data.List as List
import Text.PrettyPrint hiding (render)
import qualified Text.PrettyPrint as PP

import BNFC.Abs
import BNFC.CF
import BNFC.Backend.Common (asciiKeywords, unicodeAndSymbols)
import BNFC.Backend.OCaml.CFtoOCamlYacc (terminal)
import BNFC.Backend.OCaml.OCamlUtil (mkEsc, ocamlTokenName)
import BNFC.Lexing (mkRegMultilineComment)
import BNFC.Utils (cstring, unless)

cf2ocamllex :: String -> String -> CF -> String
cf2ocamllex _ parserMod cf = unlines $ List.intercalate [""]
  [ header parserMod cf
  , cMacros
  , rMacros cf
  , uMacros cf
  , [ PP.render $ rules cf ]
  ]

header :: String -> CF -> [String]
header parserMod cf = List.intercalate [""] . filter (not . null) $ concat
  [ [ [ "(* Lexer definition for ocamllex. *)"
      , ""
      , "(* preamble *)"
      , "{"
      , "open " ++ parserMod
      , "open Lexing"
      ]
    ]
  , hashtables cf
  , [ [ "let unescapeInitTail (s:string) : string ="
    , "  let rec unesc s = match s with"
      , "      '\\\\'::c::cs when List.mem c ['\\\"'; '\\\\'; '\\\''] -> c :: unesc cs"
      , "    | '\\\\'::'n'::cs  -> '\\n' :: unesc cs"
      , "    | '\\\\'::'t'::cs  -> '\\t' :: unesc cs"
      , "    | '\\\\'::'r'::cs  -> '\\r' :: unesc cs"
        -- "    | '\\\\'::'f'::cs  -> '\\f' :: unesc cs",  -- \f not supported by ocaml
      , "    | '\\\"'::[]    -> []"
      , "    | '\\\''::[]    -> []"
      , "    | c::cs      -> c :: unesc cs"
      , "    | _         -> []"
      , "  (* explode/implode from caml FAQ *)"
      , "  in let explode (s : string) : char list ="
      , "      let rec exp i l ="
      , "        if i < 0 then l else exp (i - 1) (s.[i] :: l) in"
      , "      exp (String.length s - 1) []"
      , "  in let implode (l : char list) : string ="
      , "      let res = Buffer.create (List.length l) in"
      , "      List.iter (Buffer.add_char res) l;"
      , "      Buffer.contents res"
      , "  in implode (unesc (List.tl (explode s)))"
      , ""
      , "let incr_lineno (lexbuf:Lexing.lexbuf) : unit ="
      , "    let pos = lexbuf.lex_curr_p in"
      , "        lexbuf.lex_curr_p <- { pos with"
      , "            pos_lnum = pos.pos_lnum + 1;"
      , "            pos_bol = pos.pos_cnum;"
      , "        }"
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
    [ unwords [ "let", table, "= Hashtbl.create", show (length syms)                  ]
    , unwords [ "let _ = List.iter (fun (kwd, tok) -> Hashtbl.add", table, "kwd tok)" ]
    , concat  [ "                  [", concat (List.intersperse ";" keyvals), "]"     ]
    ]
    where
    keyvals = map (\ s -> concat [ "(", mkEsc s, ", ", terminal cf s, ")" ]) syms

cMacros :: [String]
cMacros =
  [ "(* BNFC character classes *)"
  , "let _letter = ['A'-'Z' 'a'-'z' '\\181' '\\192' - '\\255'] # ['\\215' '\\247']    (*  isolatin1 letter *)"
  , "let _upper  = ['A'-'Z' '\\192'-'\\222'] # '\\215'      (*  capital isolatin1 letter *)"
  , "let _lower  = ['a'-'z' '\\181' '\\223'-'\\255'] # '\\247'      (*  small isolatin1 letter *)"
  , "let _digit  = ['0'-'9']                             (*  _digit *)"
  , "let _idchar = _letter | _digit | ['_' '\\'']         (*  identifier character *)"
  , "let _universal = _                                  (* universal: any character *)"
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
  [ (ocamlTokenName name, printRegOCaml reg, name, pos)
  | TokenReg n pos reg <- cfgPragmas cf
  , let name = wpThing n
  ]

-- | Make OCamlLex rule
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
-- "--" (_ # '\n')*
-- >>> mkRegexSingleLineComment "\""
-- "\"" (_ # '\n')*
mkRegexSingleLineComment :: String -> Doc
mkRegexSingleLineComment s = cstring s <+> "(_ # '\\n')*"

-- | Create regex for multiline comments.
-- >>> mkRegexMultilineComment "<!--" "-->"
-- "<!--" [^ '-']* '-' ([^ '-']+ '-')* '-' ([^ '-' '>'][^ '-']* '-' ([^ '-']+ '-')* '-' | '-')* '>'
--
-- >>> mkRegexMultilineComment "\"'" "'\""
-- "\"'" [^ '\'']* '\'' ([^ '"' '\''][^ '\'']* '\'' | '\'')* '"'
mkRegexMultilineComment :: String -> String -> Doc
mkRegexMultilineComment b e = text $ printRegOCaml $ mkRegMultilineComment b e

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
      , "let x = lexeme lexbuf in try Hashtbl.find symbol_table x with Not_found -> failwith (\"internal lexer error: reserved symbol \" ^ x ^ \" not found in hashtable\")" )
      | not (null (cfgSymbols cf))]
    ++
    -- user tokens
    [ (text n , tokenAction pos (text t)) | (n,_,t,pos) <- userTokens cf]
    ++
    -- predefined tokens
    [ ( "_letter _idchar*", tokenAction False "Ident" ) ]
    ++
    -- integers
    [ ( "_digit+", "TOK_Integer (int_of_string (lexeme lexbuf))" )
    -- doubles
    , ( "_digit+ '.' _digit+ ('e' ('-')? _digit+)?"
      , "TOK_Double (float_of_string (lexeme lexbuf))" )
    -- strings
    , ( "'\\\"' (([^ '\\\"' '\\\\' '\\n']) | ('\\\\' ('\\\"' | '\\\\' | '\\\'' | 'n' | 't' | 'r')))* '\\\"'"
      , "TOK_String (unescapeInitTail (lexeme lexbuf))" )
    -- chars
    , ( "'\\'' (([^ '\\\'' '\\\\']) | ('\\\\' ('\\\\' | '\\\'' | 'n' | 't' | 'r'))) '\\\''"
      , "TOK_Char (unescapeInitTail (lexeme lexbuf)).[0]")
    -- spaces
    , ( "[' ' '\\t' '\\r']", "token lexbuf")
    -- new lines
    , ( "'\\n'", "incr_lineno lexbuf; token lexbuf" )
    -- end of file
    , ( "eof", "TOK_EOF" )
    ]
  where
    (multilineC, singleLineC) = comments cf
    tokenAction pos t = case asciiKeywords cf of
        [] -> "TOK_" <> t <+> arg "(lexeme lexbuf)"
        _  -> "let l = lexeme lexbuf in try Hashtbl.find resword_table l with Not_found -> TOK_" <> t <+> arg "l"
      where
      arg l | pos       = text $ "((lexeme_start lexbuf, lexeme_end lexbuf), " ++ l ++ ")"
            | otherwise = text l

-------------------------------------------------------------------
-- Modified from the inlined version of former @RegToAlex@.
-------------------------------------------------------------------

-- modified from pretty-printer generated by the BNF converter

-- the top-level printing method
printRegOCaml :: Reg -> String
printRegOCaml = render . prt 0

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
  | otherwise    = ['"', c, '"']  -- ocamllex does not accept unicode character literals

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
   -- ocamllex accepts unicode characters only in string literals.
   -- Thus we translate RAlts to a disjunction rather than a character set
   RAlts str       -> prPrec i 1 $ List.intersperse "|" $ map charLiteral str
   -- RAlts str       -> concat [ ["["], map charLiteral str, ["]"] ]
   RSeqs str       -> [ show str ]
   RDigit          -> ["_digit"]
   RLetter         -> ["_letter"]
   RUpper          -> ["_upper"]
   RLower          -> ["_lower"]
   RAny            -> ["_universal"]
