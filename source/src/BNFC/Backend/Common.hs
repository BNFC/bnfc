{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions common to different backends.

module BNFC.Backend.Common
  ( unicodeAndSymbols
  , asciiKeywords
  , flexEps
  , switchByPrecedence
  )
  where

import Prelude hiding ((<>))

import Data.Bifunctor   ( second )
import Data.Char

import BNFC.CF
import BNFC.Utils       ( (>.>) )
import BNFC.PrettyPrint

-- Andreas, 2020-10-08, issue #292:
-- Since the produced lexer for Haskell and Ocaml only recognizes ASCII identifiers,
-- but cfgKeywords also contains those using unicode characters,
-- we have to reclassify any keyword using non-ASCII characters
-- as symbol.
unicodeAndSymbols :: CF -> [String]
unicodeAndSymbols cf = filter (not . all isAscii) (cfgKeywords cf) ++ cfgSymbols cf

asciiKeywords :: CF -> [String]
asciiKeywords = filter (all isAscii) . cfgKeywords

-- | Representation of the empty word as Flex regular expression
flexEps :: String
flexEps = "[^.\\n]?"

-- | Helper function for c-like languages that generates the code printing
-- the list separator according to the given precedence level:
--
-- >>> let my_render c = "my_render(\"" <> text c <> "\")"
-- >>> renderListSepByPrecedence "x" my_render []
-- <BLANKLINE>
--
-- >>> renderListSepByPrecedence "x" my_render [(0,",")]
-- my_render(",");
--
-- >>> renderListSepByPrecedence "x" my_render [(3,";"), (1, "--")]
-- switch(x)
-- {
--   case 3: my_render(";"); break;
--   case 1: my_render("--"); break;
-- }
renderListSepByPrecedence
  :: Doc                 -- ^ Name of the coercion level variable
  -> (String -> Doc)     -- ^ render function
  -> [(Integer, String)] -- ^ separators by precedence
  -> Doc
renderListSepByPrecedence var render =
  vcat . switchByPrecedence var . map (second $ render >.> (<> ";"))

-- Note (Andreas, 2021-05-02):
-- @renderListSepByPrecedence@ did not account for mixfix lists (issue #358)
-- and has been replaced by the more general @switchByPrecedence@.

switchByPrecedence
  :: Doc              -- ^ Name of the coercion level variable/
  -> [(Integer, Doc)] -- ^ Content by precedence.
  -> [Doc]
switchByPrecedence var = filter (not . isEmpty . snd) >.> \case
  []        -> []
  [(_,doc)] -> [ doc  ]
  ds        ->
    [ "switch(" <> var <> ")"
    , codeblock 2
      [ "case" <+> integer i <:> doc <+> "break;" | (i, doc) <- ds ]
    -- , codeblock 2 $ concat
    --   [ [ "case" <+> integer i <:> doc <+> "break;" |     (i, doc) <- init ds ]
    --   , [ "default" <:> doc                         | let (i, doc) =  last ds ]
    --   ]
    ]
    where
    a <:> b = a <> ":" <+> b
