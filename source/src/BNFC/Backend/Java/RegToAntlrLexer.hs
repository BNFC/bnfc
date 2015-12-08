module BNFC.Backend.Java.RegToAntlrLexer (printRegJLex, escapeChar) where

-- modified from RegToJLex.hs

import AbsBNF

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
  prtList :: [a] -> [String]
  prtList = concatMap (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ c = [escapeChar c]
  prtList = map (concat . prt 0)

escapeChar :: Char -> String
escapeChar x | x `elem` reserved = '\\' : [x]
escapeChar x = [x]

-- Characters that must be escaped in ANTLR regular expressions
reserved :: [Char]
reserved = ['\'','\\']

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if j<i then parenth else id

instance Print Ident where
  prt _ (Ident i) = [i]

instance Print Reg where
  prt i e = case e of
   RSeq reg0 reg
              -> prPrec i 2 (concat [prt 2 reg0 , prt 3 reg])
   RAlt reg0 reg
              -> prPrec i 1 (concat [prt 1 reg0 , ["|"] , prt 2 reg])
   -- JLex does not support set difference
   --RMinus reg0 reg -> prPrec i 1 (concat [prt 2 reg0 , ["#"] , prt 2 reg])
   RMinus reg0 REps -> prt i reg0 -- REps is identity for set difference
   RMinus RAny reg@(RChar _)
              ->  prPrec i 3 (concat [["~["],prt 0 reg,["]"]])
   RMinus RAny (RAlts str)
              ->  prPrec i 3 (concat [["~["],prt 0 str,["]"]])
   RMinus _ _ -> error "Antlr does not support general set difference"
   RStar reg  -> prPrec i 3 (concat [prt 3 reg , ["*"]])
   RPlus reg  -> prPrec i 3 (concat [prt 3 reg , ["+"]])
   ROpt reg   -> prPrec i 3 (concat [prt 3 reg , ["?"]])
   REps       -> prPrec i 3 [""]
   RChar c    -> prPrec i 3 (concat [["'"], prt 0 c, ["'"]])
   RAlts str  -> prPrec i 3 (concat [["["],prt 0 str,["]"]])
   RSeqs str  -> prPrec i 2 (concatMap (prt 0) str)
   RDigit     -> prPrec i 3 ["DIGIT"]
   RLetter    -> prPrec i 3 ["LETTER"]
   RUpper     -> prPrec i 3 ["CAPITAL"]
   RLower     -> prPrec i 3 ["SMALL"]
   RAny       -> prPrec i 3 ["[\\u0000-\\u00FF]"]
