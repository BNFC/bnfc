module RegToGPLEX (printRegGPLEX) where

-- modified from RegToFlex

import AbsBNF
import Data.Char

-- the top-level printing method
printRegGPLEX :: Reg -> String
printRegGPLEX = render . prt 0

-- you may want to change render and parenth

render :: [String] -> String
render = rend (0::Int) where
  rend i ss = case ss of
    "["      :ts -> cons "["  $ rend i ts
    "("      :ts -> cons "("  $ rend i ts
    t  : "," :ts -> cons t    $ space "," $ rend i ts
    t  : ")" :ts -> cons t    $ cons ")"  $ rend i ts
    t  : "]" :ts -> cons t    $ cons "]"  $ rend i ts
    t        :ts -> space t   $ rend i ts
    _            -> ""
  cons s t  = s ++ t
  new i s   = s
  space t s = if null s then t else t ++ s

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
  prt _ c = [[c]]
  prtList s = map (concat . prt 0) s

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if j<i then parenth else id

instance Print Ident where
  prt _ (Ident i) = [i]

instance Print Reg where
  prt i e = case e of
   RSeq reg0 reg   -> prPrec i 2 (concat [prt 2 reg0 , prt 3 reg])
   RAlt reg0 reg   -> prPrec i 1 (concat [prt 1 reg0 , ["|"] , prt 2 reg])
   RMinus reg0 reg -> prPrec i 1 (concat [prt 2 reg0 , ["#"] , prt 2 reg])
   RStar reg       -> prPrec i 3 (concat [prt 3 reg , ["*"]])
   RPlus reg       -> prPrec i 3 (concat [prt 3 reg , ["+"]])
   ROpt reg        -> prPrec i 3 (concat [prt 3 reg , ["?"]])
   REps            -> prPrec i 3 (["[^.]"])
   RChar c         -> prPrec i 3 (prt 0 [mkEsc [c]])
   RAlts str       -> prPrec i 3 (concat [["["], prt 0 $ mkEsc str, ["]"]])
   RSeqs str       -> prPrec i 2 (concat (map (prt 0) $ mkEsc str))
   RDigit          -> prPrec i 3 (concat [["{digit}"]])
   RLetter         -> prPrec i 3 (concat [["{alpha}"]])
   RUpper          -> prPrec i 3 (concat [["{alphaCapital}"]])
   RLower          -> prPrec i 3 (concat [["{alphaSmall}"]])
   RAny            -> prPrec i 3 (concat [["."]])

-- Handle special characters in regular expressions.
mkEsc :: String -> String
mkEsc = concatMap escChar
    where escChar c
	      | c `elem` "$+-*=<>[](){}!?.,;:^~|&%#/\\$_@\"" = '\\':[c]
	      | otherwise = [c]
