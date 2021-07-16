{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.Java.RegToJLex (printRegJLex, escapeChar) where

import Data.Char           (ord, showLitChar)

import BNFC.Abs            (Identifier(..), Reg(..))
import BNFC.Options        (JavaLexerParser(..))
import BNFC.Backend.Common (flexEps)

-- | Print a regular expression for the Java lexers.

printRegJLex :: JavaLexerParser -> Reg -> String
printRegJLex lexer reg = prt lexer 0 reg ""

class Print a where
  prt     :: JavaLexerParser -> Int -> a -> ShowS
  prtList :: JavaLexerParser -> [a] -> ShowS
  prtList lexer xs s = foldr (prt lexer 0) s xs
  -- OR: prtList lexer = foldr (.) id . map (prt lexer 0)

instance Print a => Print [a] where
  prt lexer _ = prtList lexer

instance Print Char where
  prt lexer _ c = showString $ escapeChar lexer c

escapeChar :: JavaLexerParser -> Char -> String
escapeChar _ '^' = "\\x5E" -- special case, since \^ is a control character escape
escapeChar JFlexCup x | x `elem` jflexReserved = '\\' : [x]
escapeChar _ x
  | x `elem` jlexReserved = '\\' : [x]
  | ord x >= 255          = [x]
  | otherwise             = showLitChar x ""

-- Characters that must be escaped in JLex regular expressions
jlexReserved :: [Char]
jlexReserved = ['?','*','+','|','(',')','^','$','.','[',']','{','}','"','\\']

jflexReserved :: [Char]
jflexReserved = '~':'!':'/':[]  -- plus the @jlexReserved@, but they are tested separately

instance Print Identifier where
  prt _ _ (Identifier (_, x)) = showString x

instance Print Reg where
  prt lexer i = \case
    RSeq reg1 reg2          -> showParen (i > 2) $ prt lexer 2 reg1 . prt lexer 3 reg2
    RAlt reg1 reg2          -> showParen (i > 1) $ prt lexer 1 reg1 . showChar '|' . prt lexer 2 reg2

    -- JLex does not support set difference in general
    RMinus reg0 REps        -> prt lexer i reg0 -- REps is identity for set difference
    RMinus RAny reg@RChar{} -> showParen (i > 3) $ showString "[^" . prt lexer 0 reg . showString "]"
    RMinus RAny (RAlts str) -> showParen (i > 3) $ showString "[^" . prt lexer 0 str . showString "]"
    -- FIXME: maybe we could add cases for char - RDigit, RLetter etc.
    RMinus _ _              -> error $ "J[F]Lex does not support general set difference"

    RStar reg               -> showParen (i > 3) $ prt lexer 3 reg . showChar '*'
    RPlus reg               -> showParen (i > 3) $ prt lexer 3 reg . showChar '+'
    ROpt reg                -> showParen (i > 3) $ prt lexer 3 reg . showChar '?'
    REps                    -> showParen (i > 3) $ showString flexEps
    RChar c                 -> showParen (i > 3) $ prt lexer 0 c
    RAlts str               -> showParen (i > 3) $ showChar '[' . prt lexer 0 str . showChar ']'
    RSeqs str               -> showParen (i > 2) $ prt lexer 0 str
    RDigit                  -> showParen (i > 3) $ showString "{DIGIT}"
    RLetter                 -> showParen (i > 3) $ showString "{LETTER}"
    RUpper                  -> showParen (i > 3) $ showString "{CAPITAL}"
    RLower                  -> showParen (i > 3) $ showString "{SMALL}"
    RAny                    -> showParen (i > 3) $ showChar '.'
