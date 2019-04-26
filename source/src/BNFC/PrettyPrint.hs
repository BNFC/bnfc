{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Extends ''Text.PrettyPrint''.

module BNFC.PrettyPrint
  ( module Text.PrettyPrint
  , module BNFC.PrettyPrint
  ) where

import Text.PrettyPrint

-- | Put 'parens' around document if given condition is true.
--
-- >>> parensIf True "foo"
-- (foo)
--
-- >>> parensIf False "bar"
-- bar
--
parensIf :: Bool -> Doc -> Doc
parensIf = \case
  True  -> parens
  False -> id

-- | Separate vertically by a blank line.
--
-- >>> "foo" $++$ "bar"
-- foo
-- <BLANKLINE>
-- bar
--
-- >>> "foo" $++$ empty
-- foo
--
($++$) :: Doc -> Doc -> Doc
d $++$ d'
  | isEmpty d  = d'
  | isEmpty d' = d
  | otherwise  = d $+$ "" $+$ d'

-- | List version of '$++$'.
--
-- >>> vsep [ "foo", nest 4 "bar" ]
-- foo
-- <BLANKLINE>
--     bar
--
-- >>> vsep []
-- <BLANKLINE>
--
vsep :: [Doc] -> Doc
vsep = foldl ($++$) empty

-- | List version of 'PrettyPrint.$+$'.
--
-- >>> vcat' [text "abc", nest 4 (text "def")]
-- abc
--     def
--
vcat' :: [Doc] -> Doc
vcat' = foldl ($+$) empty

-- | Pretty print separator with a dot.
--
-- >>> "abc" <.> "py"
-- abc.py
--
(<.>) :: Doc -> Doc -> Doc
a <.> b = hcat [ a , text "." , b ]
  -- Andreas A, 2019-02-07: avoiding <> due to clash with Semigroup

-- | Pretty print separator with = (for assignments...).
--
-- >>> "a" <=> "123"
-- a = 123
--
(<=>) :: Doc -> Doc -> Doc
a <=> b = a <+> text "=" <+> b

-- | Print a list of 0-1 elements on the same line as some preamble
--   and from 2 elements on the following lines, indented.
--
-- >>> prettyList 2 ("foo" <+> equals) lbrack rbrack comma []
-- foo = []
-- >>> prettyList 2 ("foo" <+> equals) lbrack rbrack comma [ "a" ]
-- foo = [a]
-- >>> prettyList 2 ("foo" <+> equals) lbrack rbrack comma [ "a", "b" ]
-- foo =
--   [ a
--   , b
--   ]
--
-- Used in the Agda backend.
prettyList
  :: Int   -- ^ Indentation.
  -> Doc   -- ^ Preamble.
  -> Doc   -- ^ Left parenthesis.
  -> Doc   -- ^ Right parenthesis.
  -> Doc   -- ^ Separator (usually not including spaces).
  -> [Doc] -- ^ List item.
  -> Doc
prettyList n pre lpar rpar sepa = \case
  []     -> pre <+> hcat [ lpar, rpar ]
  [d]    -> pre <+> hcat [ lpar, d, rpar ]
  (d:ds) -> vcat . (pre :) . map (nest n) . concat $
    [ [ lpar <+> d ]
    , map (sepa <+>) ds
    , [ rpar ]
    ]

-- | Code block. A block of C/Java code, surrounded by {} and indented.
--
-- >>> codeblock 4 ["abc", "def"]
-- {
--     abc
--     def
-- }
--
--   Used in the C backend.
codeblock :: Int -> [Doc] -> Doc
codeblock indent code = lbrace $+$ nest indent (vcat code) $+$ rbrace
