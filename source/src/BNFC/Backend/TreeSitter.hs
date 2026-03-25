{-
    BNF Converter: TreeSitter Grammar Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Description   : This module generates the grammar.js input file for
                    tree-sitter.

    Author        : Kangjing Huang (huangkangjing@gmail.com)
    Created       : 23 Nov, 2023

-}

module BNFC.Backend.TreeSitter where

import Prelude hiding ((<>))

import BNFC.Backend.Base
import BNFC.Backend.TreeSitter.CFtoTreeSitter (cfToTreeSitter)
import BNFC.Backend.Common.Makefile(mkMakefile, mkRule, mkVar)
import BNFC.GetCF(fixTokenCats)
import BNFC.Utils(cstring, snakeCase_, camelCase_)
import BNFC.CF
import BNFC.Options hiding (Backend)
import BNFC.PrettyPrint

-- | Entry point: create grammar.js file
makeTreeSitter :: SharedOptions -> CF -> Backend
makeTreeSitter opts cf = do

  mkfile "grammar.js" comment $
    render (cfToTreeSitter name wordCat cf)

  mkfile "tree-sitter.json" (const "") $
    render (treeSitterJson name)

  mkMakefile (optMake opts) $
    treeSitterMakefile

  where
    name = snakeCase_ (lang opts)
    wordCat = fixTokenCats (tokenNames cf) (strToCat (treeSitterWord opts))

comment :: String -> String
comment = ("// " ++)

treeSitterMakefile :: String -> Doc
treeSitterMakefile makefileName = vcat'
  [ mkVar "TREE_SITTER" "tree-sitter"
  , ".PHONY: parse clean"
  , mkRule "src/parser.c" ["grammar.js"] ["$(TREE_SITTER) generate"]
  , mkRule "parse" ["src/parser.c"] ["$(TREE_SITTER) parse --cst"]

  , mkRule "clean" [] ["rm -rfv src/grammar.json src/node-types.json src/parser.c src/tree_sitter"]
  , mkRule "distclean" ["clean"] $
      [ "rm -rfv grammar.js tree-sitter.json " ++ makefileName
      , "rm -rv src  # no -f, so only removes src if it was emptied by clean"
      ]
  ]

treeSitterJson :: String -> Doc
treeSitterJson name =
  jsonObject
    [ p "$schema" $ str "https://tree-sitter.github.io/tree-sitter/assets/schemas/config.schema.json"
    , p "grammars" $ jsonArray1 $ jsonObject
      [ p "name" $ str $ snakeCase_ name
      , p "camelcase" $ str $ camelCase_ name
      , p "scope" $ str $ "source." ++ snakeCase_ name
      ]
    , p "metadata" $ jsonObject
      [ p "version" $ str "0.1.0"
      , p "authors" $ jsonArray1 $ jsonObject [p "name" $ str "BNFC"]
      ]
    , p "bindings" $ jsonObject
      [ p "c" "false"
      , p "go" "false"
      , p "java" "false"
      , p "node" "false"
      , p "python" "false"
      , p "rust" "false"
      , p "swift" "false"
      , p "zig" "false"
      ]
    ]
  where
    str = cstring
    p = (,)

jsonLines :: [Doc] -> Doc
jsonLines = nest 2 . sep . punctuate ","

jsonObject :: [(String, Doc)] -> Doc
jsonObject pairs = "{" $$ jsonLines (map jsonPair pairs) <+> "}"
  where
    jsonPair (k, v) = cstring k <> ":" <+> v

jsonArray :: [Doc] -> Doc
jsonArray elems = "[" $$ jsonLines elems <+> "]"

jsonArray1 :: Doc -> Doc
jsonArray1 = jsonArray . pure
