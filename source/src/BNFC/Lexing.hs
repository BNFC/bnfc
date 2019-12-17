module BNFC.Lexing
    ( mkLexer, LexType(..), mkRegMultilineComment ) where

import Prelude hiding ((<>))

-- import Control.Arrow ( (&&&) )
-- import Data.List     ( inits )

import AbsBNF        ( Reg(..) )
import BNFC.CF
import BNFC.Regex    ( simpReg )
import BNFC.Utils    ( unless  )

-- $setup
-- >>> import PrintBNF
-- >>> let p = putStrLn . concat . words . printTree :: Reg -> IO ()

-- Abstract lexer

data LexType = LexComment | LexToken String | LexSymbols

mkLexer :: CF -> [(Reg, LexType)]
mkLexer cf = concat
    -- comments
  [ [ (mkRegSingleLineComment s, LexComment) | s <- snd (comments cf) ]
  , [ (mkRegMultilineComment b e, LexComment) | (b,e) <- fst (comments cf) ]
    -- user tokens
  , [ (reg, LexToken name) | (name, reg) <- tokenPragmas cf]
    -- predefined tokens
  , [ ( regIdent, LexToken "Ident" ) ]
    -- Symbols
  , unless (null $ cfgSymbols cf) [ (foldl1 RAlt (map RSeqs (cfgSymbols cf)), LexSymbols ) ]
    -- Built-ins
  , [ ( regInteger, LexToken "Integer")
    , ( regDouble , LexToken "Double" )
    , ( regString , LexToken "String" )
    , ( regChar   , LexToken "Char"   )
    ]
  ]


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
            <> RStar ((RAny `RMinus` RAlts "\"\\")
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
-- {"--"}char*'\n'
mkRegSingleLineComment :: String -> Reg
mkRegSingleLineComment s = RSeqs s <> RStar RAny <> RChar '\n'

-- WRONG, trips on <!-- ----> (for second example, see also regression test 108):
--
-- -- | Create regex for multiline comments
-- -- >>> p $ mkRegMultilineComment "<" ">"
-- -- '<'(char-'>'|'\n')*'>'
-- -- >>> p $ mkRegMultilineComment "<!--" "-->"
-- -- {"<!--"}(char-'-'|'\n'|'-'(char-'-'|'\n')|{"--"}(char-'>'|'\n'))*'-'*{"-->"}
-- mkRegMultilineComment :: String -> String -> Reg
-- mkRegMultilineComment b e =
--     foldl1 RSeq $ concat
--       [ lit b
--       , [ RStar (foldl1 RAlt subregex) | not $ null subregex ]
--       , [ RStar (RChar (head e))       | length e >= 2       ]
--       , lit e
--       ]
--   where
--     lit :: String -> [Reg]
--     lit "" = []
--     lit [c] = [RChar c]
--     lit s = [RSeqs s]
--     prefixes = map (init &&& last) (drop 1 (inits e))
--     subregex =
--       [ foldr RSeq ((RAny `RMinus` RChar s) `RAlt` RChar '\n') $ lit ss
--       | (ss,s) <- prefixes
--       ]

-- | Create regex for multiline comments.
--
-- >>> p $ mkRegMultilineComment "<" ">"
-- '<'(char-'>')*'>'
--
-- >>> p $ mkRegMultilineComment "<!--" "-->"
-- {"<!--"}(char-'-')*'-'((char-'-')+'-')*'-'('-'|(char-["->"])(char-'-')*'-'((char-'-')+'-')*'-')*'>'
--
mkRegMultilineComment :: String -> String -> Reg
mkRegMultilineComment b []       = RSeqs b
mkRegMultilineComment b (a:rest) = simpReg $ RSeqs b `RSeq` fromStart
  where
  notA                       = RAny `RMinus` RChar a
  goA                        = RStar notA `RSeq` RChar a
  (fromStart, _, _)          = foldl f (goA, REps, []) rest
  -- Build up automaton states Start, A, ...ys..., x, ...
  f (fromStart, fromA, ys) x = (advance fromStart, advance fromA, x:ys)
    where
    advance from = (from `RSeq` RStar idle) `RSeq` RChar x
    idle         = foldl1 RAlt $ concat
      -- cannot advance, ...
      [ [ RChar a              | a /= x, all (a ==) ys            ] -- but can stay
      , [ RChar a `RSeq` fromA | a /= x, null ys || any (a /=) ys ] -- but can fall back to A
      , [ (RAny `RMinus` RAlts [x,a]) `RSeq` fromStart            ] -- neither, need to restart
      ]
