{-# LANGUAGE NoImplicitPrelude #-}

module BNFC.Lexing
    ( mkLexer, LexType(..) ) where

import Prelude'

import Control.Arrow ((&&&))
import Data.List (inits)
import AbsBNF (Reg(..))
import BNFC.CF

-- $setup
-- >>> import PrintBNF
-- >>> let p = putStrLn . concat . words . printTree :: Reg -> IO ()

-- Abstract lexer

data LexType = LexComment | LexToken String | LexSymbols

mkLexer :: CF -> [(Reg, LexType)]
mkLexer cf =
    -- comments
    [ (mkRegSingleLineComment s, LexComment) | s <- snd (comments cf) ]
    ++
    [ (mkRegMultilineComment b e, LexComment) | (b,e) <- fst (comments cf) ]
    ++
    -- user tokens
    [ (reg, LexToken name) | (Cat name, reg) <- tokenPragmas cf]
    ++
    -- predefined tokens
    [ ( regIdent, LexToken "Ident" ) ]
    ++
    -- Symbols
    [ (foldl1 RAlt (map RSeqs (cfgSymbols cf)), LexSymbols ) ]
    ++
    -- Built-ins
    [ ( regInteger, LexToken "Integer")
    , ( regDouble, LexToken "Double" )
    , ( regString, LexToken "String" )
    , ( regChar, LexToken "Char" ) ]


(<>) = RSeq
(<|>) = RAlt

-- Bult-in tokens
-- the tests make sure that they correspond to what is in the LBNF reference

-- | Ident regex
-- >>> p regIdent
-- letter(letter|digit|'_'|'\'')*
regIdent :: Reg
regIdent = RLetter <> RStar (RLetter <|> RDigit <|> RChar '_' <|> RChar '\'')

-- | Integer regex
-- >>> p regInteger
-- digit+
regInteger :: Reg
regInteger = RPlus RDigit

-- | String regex
-- >>> p regString
-- '"'(char-["\"\\"]|'\\'["\"\\nt"])*'"'
regString :: Reg
regString = RChar '"'
            <> RStar ( RMinus RAny (RAlts "\"\\")
                       <|> (RChar '\\' <> RAlts "\"\\nt"))
            <> RChar '"'

-- | Char regex
-- >>> p regChar
-- '\''(char-["'\\"]|'\\'["'\\nt"])'\''
regChar :: Reg
regChar = RChar '\''
          <> (RMinus RAny (RAlts "'\\") <|> (RChar '\\' <> RAlts "'\\nt"))
          <> RChar '\''

-- | Double regex
-- >>> p regDouble
-- digit+'.'digit+('e''-'?digit+)?
regDouble :: Reg
regDouble = RPlus RDigit <> RChar '.' <> RPlus RDigit
            <> ROpt (RChar 'e' <> ROpt (RChar '-') <> RPlus RDigit)

-- | Create regex for single line comments
-- >>> p $ mkRegSingleLineComment "--"
-- {"--"}(char*'\n')
mkRegSingleLineComment :: String -> Reg
mkRegSingleLineComment s = RSeq (RSeqs s) (RSeq (RStar RAny) (RChar '\n'))

-- | Create regex for multiline comments
-- >>> p $ mkRegMultilineComment "<" ">"
-- '<'((char|'\n')-'>')*'>'
-- >>> p $ mkRegMultilineComment "<!--" "-->"
-- {"<!--"}((char|'\n')-'-'|'-'((char|'\n')-'-')|{"--"}((char|'\n')-'>'))*'-'*{"-->"}
mkRegMultilineComment :: String -> String -> Reg
mkRegMultilineComment b e =
    rseq $ concat [
      lit b
      , [RStar (foldl1 RAlt subregex)]
      , [ RStar (RChar (head e)) | length e > 1 ]
      , lit e]
  where
    rseq = foldl1 RSeq
    lit :: String -> [Reg]
    lit "" = []
    lit [c] = [RChar c]
    lit s = [RSeqs s]
    prefixes = map (init &&& last) (drop 1 (inits e))
    subregex = [rseq (lit ss ++ [RMinus (RAlt RAny (RChar '\n')) (RChar s)]) | (ss,s) <- prefixes]
