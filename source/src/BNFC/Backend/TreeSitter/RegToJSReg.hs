{-
    BNF Converter: TreeSitter Grammar Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Description   : This module converts BNFC Reg to Javascript regular
                    expressions that is used in

    Author        : Kangjing Huang (huangkangjing@gmail.com)
    Created       : 23 Nov, 2023

-}
{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.TreeSitter.RegToJSReg (printRegJSReg) where

import BNFC.Abs

printRegJSReg :: Reg -> String
printRegJSReg r = "/" ++ render (prt 0 r) ++ "/"

-- There is no space-based formatting for javascript regex
-- We just concat everything together
render :: [String] -> String
render = concat

parenth :: [String] -> [String]
parenth s = ["("] ++ s ++ [")"]

-- Printer class
class Print a where
  prt :: Int -> a -> [String]

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if i > j then parenth else id

-- | reserved characters for Javascript regex format, sourced from
--   https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions#escaping
reserved :: String
reserved = ".*+?^${}()|[]\\/"

-- | reserved characters for alt expressions in Javascript regex format
reservedAlt :: String
reservedAlt = "^[]-\\/"

-- | Pattern for matching empty string in Javascript regex format
emptyPat :: String
emptyPat = " [^\\u0000-\\uFFFF]?"

-- | escape character according to Javascript regex format
escapeCharFrom :: String -> Char -> String
escapeCharFrom reservedChars x
  | x `elem` reservedChars = '\\' : [x]
  | otherwise = [x]

escapeChar :: Char -> String
escapeChar = escapeCharFrom reserved

escapeStr :: String -> String
escapeStr = concatMap escapeChar

escapeCharAlt :: Char -> String
escapeCharAlt = escapeCharFrom reservedAlt

escapeStrAlt :: String -> String
escapeStrAlt = concatMap escapeChar

instance Print Identifier where
  prt _ (Identifier (_, i)) = [i]

instance Print Reg where
  prt i = \case
    RSeq reg0 reg -> prPrec i 2 $ prt 2 reg0 ++ prt 3 reg
    RAlt reg0 reg -> prPrec i 1 $ prt 1 reg0 ++  ["|"] ++ prt 2 reg
    -- Javascript regex does not support general set difference
    RMinus reg0 REps -> prt i reg0 -- REps is identity for difference
    RMinus RAny (RChar c) -> ["[^" ++ escapeCharAlt c ++ "]"]
    RMinus RAny (RAlts s) -> ["[^" ++ escapeStrAlt s ++ "]"]
    -- TODO: It is possible to use external scanners in tree-sitter:
    -- https://tree-sitter.github.io/tree-sitter/creating-parsers#external-scanners
    -- to support general set differences in the future
    RMinus _ _ -> error "Javascript regex does not support general set differences"
    RStar reg -> prt 3 reg ++ ["*"]
    RPlus reg -> prt 3 reg ++ ["+"]
    ROpt reg -> prt 3 reg ++ ["?"]
    REps -> [emptyPat]
    RChar c -> [escapeChar c]
    RAlts str -> ["[" ++ escapeStrAlt str ++ "]"]
    RSeqs str -> [escapeStr str]
    RDigit -> ["\\d"]
    RLetter -> ["[a-zA-Z]"]
    RUpper -> ["[A-Z]"]
    RLower -> ["[a-z]"]
    RAny -> ["."]
