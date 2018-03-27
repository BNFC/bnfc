{-# LANGUAGE NoImplicitPrelude #-}

-- Extends Text.PrettyPrint
module BNFC.PrettyPrint
  ( module Text.PrettyPrint
  , (<.>)
  , codeblock
  , vsep
  , (<=>)
  ) where

import Prelude'

import Text.PrettyPrint

-- | Pretty print separator with a dot
-- >>> "abc" <.> "py"
-- abc.py
(<.>) :: Doc -> Doc -> Doc
a <.> b = a <> "." <> b

-- | Code block. A bloc of code, surrounded by {} and indented.
-- >>> codeblock 4 ["abc", "def"]
-- {
--     abc
--     def
-- }
codeblock :: Int -> [Doc] -> Doc
codeblock indent code = lbrace $+$ nest indent (vcat code) $+$ rbrace

-- | List version of prettyPrint $+$
-- >>> vsep [text "abc", nest 4 (text "def")]
-- abc
--     def
vsep :: [Doc] -> Doc
vsep = foldl ($+$) empty

-- | Pretty print separator with = (for assignments...)
-- >>> "a" <=> "123"
-- a = 123
(<=>) :: Doc -> Doc -> Doc
a <=> b = a <+> "=" <+> b
