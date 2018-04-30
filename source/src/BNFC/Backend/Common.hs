{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions common to different backends.

module BNFC.Backend.Common (renderListSepByPrecedence) where

import Prelude'

import BNFC.PrettyPrint

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
