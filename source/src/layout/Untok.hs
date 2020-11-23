{-
    BNF Converter: untokenizer
    Copyright (C) 2004  Author: Aarne Ranta

-}

module Untok where

import LexAlfa2
import Alex

untok :: [Token] -> String
untok = unto (1,1) where
  unto _  [] = ""
  unto (l0,c0) (t@(PT (Pn _ l c) _) : ts) =
    let s  = prToken t
        ns = l - l0
        ls = replicate ns '\n'
        cs = replicate (if ns == 0 then c - c0 else c-1) ' '
    in ls ++ cs ++ s ++ unto (l,c + length s) ts
