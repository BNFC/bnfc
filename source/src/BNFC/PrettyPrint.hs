-- | Extends ''Text.PrettyPrint''.

module BNFC.PrettyPrint
  ( module Text.PrettyPrint
  , (<.>)
  , codeblock
  , vcat'
  , (<=>)
  ) where

import Text.PrettyPrint

-- | Pretty print separator with a dot
-- >>> "abc" <.> "py"
-- abc.py
(<.>) :: Doc -> Doc -> Doc
a <.> b = hcat [ a , text "." , b ]
  -- Andreas A, 2019-02-07: avoiding <> due to clash with Semigroup

-- | Code block. A bloc of code, surrounded by {} and indented.
-- >>> codeblock 4 ["abc", "def"]
-- {
--     abc
--     def
-- }
codeblock :: Int -> [Doc] -> Doc
codeblock indent code = lbrace $+$ nest indent (vcat code) $+$ rbrace

-- | List version of prettyPrint $+$
-- >>> vcat' [text "abc", nest 4 (text "def")]
-- abc
--     def
vcat' :: [Doc] -> Doc
vcat' = foldl ($+$) empty

-- | Pretty print separator with = (for assignments...)
-- >>> "a" <=> "123"
-- a = 123
(<=>) :: Doc -> Doc -> Doc
a <=> b = a <+> text "=" <+> b
