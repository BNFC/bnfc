module BNFC.Backend.Antlr.RegToAntlrLexer (printRegJLex, escapeCharInSingleQuotes) where

-- modified from RegToJLex.hs

import Data.Char (ord)
import Numeric (showHex)

import BNFC.Abs

-- the top-level printing method
printRegJLex :: Reg -> String
printRegJLex = render . prt 0

-- you may want to change render and parenth

render :: [String] -> String
render = rend (0 :: Int) where
  rend i ss = case ss of
    "["      :ts -> cons "["  $ rend i ts
    "("      :ts -> cons "("  $ rend i ts
    t  : "," :ts -> cons t    $ space "," $ rend i ts
    t  : ")" :ts -> cons t    $ cons ")"  $ rend i ts
    t  : "]" :ts -> cons t    $ cons "]"  $ rend i ts
    t        :ts -> space t   $ rend i ts
    _            -> ""
  cons s t  = s ++ t
  space t s = if null s then t else t ++ s

parenth :: [String] -> [String]
parenth ss = ["("] ++ ss ++ [")"]

-- the printer class does the job
class Print a where
  prt :: Int -> a -> [String]

-- | Print char according to ANTLR regex format.
escapeChar :: [Char] -> Char -> String
escapeChar reserved x
  | x `elem` reserved  = '\\' : [x]
  | i >= 65536         = "\\u{" ++ h ++ "}"
  | i >= 256 || i < 32 = "\\u" ++ replicate (4 - length h) '0' ++ h
  | otherwise          = [x]  -- issue #329, don't escape in the usual way!
  where
  i = ord x
  h = showHex i ""

-- | Escape character for use inside single quotes.
escapeCharInSingleQuotes :: Char -> String
escapeCharInSingleQuotes = escapeChar ['\'','\\']

-- The ANTLR definition of what can be in a [char set] is here:
-- https://github.com/antlr/antlr4/blob/master/doc/lexer-rules.md#lexer-rule-elements
-- > The following escaped characters are interpreted as single special characters:
-- > \n, \r, \b, \t, \f, \uXXXX, and \u{XXXXXX}.
-- > To get ], \, or - you must escape them with \.

-- | Escape character for use inside @[char set]@.
escapeInCharSet :: Char -> String
escapeInCharSet = escapeChar [ ']', '\\', '-' ]

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if j<i then parenth else id

instance Print Identifier where
  prt _ (Identifier (_, i)) = [i]

instance Print Reg where
  prt i e = case e of
   RSeq reg0 reg
              -> prPrec i 2 (concat [prt 2 reg0 , [" "], prt 3 reg])
   RAlt reg0 reg
              -> prPrec i 1 (concat [prt 1 reg0 , ["|"] , prt 2 reg])
   RMinus reg0 REps -> prt i reg0 -- REps is identity for set difference
   RMinus RAny (RChar c)
              -> ["~'", escapeCharInSingleQuotes c, "'"]
   RMinus RAny (RAlts str)
              -> concat [["~["], map escapeInCharSet str ,["]"]]
   RMinus _ _ -> error "Antlr does not support general set difference"
   RStar reg  -> prt 3 reg ++ ["*"]
   RPlus reg  -> prt 3 reg ++ ["+"]
   ROpt reg   -> prt 3 reg ++ ["?"]
   REps       -> [""]
   RChar c    -> ["'", escapeCharInSingleQuotes c, "'"]
   RAlts str  -> concat [ ["["], map escapeInCharSet str, ["]"] ]
   RSeqs str  -> prPrec i 2 $ map show str
   RDigit     -> ["DIGIT"]
   RLetter    -> ["LETTER"]
   RUpper     -> ["CAPITAL"]
   RLower     -> ["SMALL"]
   RAny       -> ["[\\u0000-\\u00FF]"]
