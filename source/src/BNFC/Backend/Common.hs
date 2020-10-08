{-# LANGUAGE OverloadedStrings #-}

-- | Functions common to different backends.

module BNFC.Backend.Common where

import Prelude hiding ((<>))

import Data.Char

import BNFC.CF
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
--   default: my_render("--");
-- }
renderListSepByPrecedence :: Doc                 -- ^ Name of the coercion level variable
                         -> (String -> Doc)     -- ^ render function
                         -> [(Integer, String)] -- ^ separators by precedence
                         -> Doc
renderListSepByPrecedence _ _ [] = empty
renderListSepByPrecedence _ render [(_,sep)] = render sep <> ";"
renderListSepByPrecedence var render ss = "switch(" <> var <> ")" $$ codeblock 2
    ( ["case" <+> integer i <:> render sep <>"; break;" | (i, sep) <- init ss]
    ++ ["default" <:> render sep <>";" | let (_,sep) = last ss])
  where
    a <:> b = a <> ":" <+> b
