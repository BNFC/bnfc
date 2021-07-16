{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Generate main/test module for OCaml
    Copyright (C) 2005  Author:  Kristofer Johannisson

-}

module BNFC.Backend.OCaml.CFtoOCamlTest where

import Prelude hiding ((<>))

import Text.PrettyPrint

import BNFC.CF
import BNFC.Backend.OCaml.OCamlUtil
import BNFC.Backend.OCaml.CFtoOCamlYacc    (epName)
import BNFC.Backend.OCaml.CFtoOCamlPrinter (prtFun)
import BNFC.Backend.OCaml.CFtoOCamlShow    (showsFunQual)

-- | OCaml comment
-- >>> comment "I'm a comment"
-- (* I'm a comment *)
comment :: Doc -> Doc
comment d = "(*" <+> d <+> "*)"

-- | Generate a test program in OCaml
ocamlTestfile :: String -> String -> String -> String -> String -> CF -> Doc
ocamlTestfile absM lexM parM printM showM cf =
    let
        cat         = firstEntry cf
        qualify q x = concat [ q, ".", x ]
        lexerName   = text $ qualify lexM "token"
        parserName  = text $ qualify parM $ epName cat
        printerName = hsep $ map (text . qualify printM) [ "printTree", prtFun cat ]
        showFun     = parens . hsep $
          [ "fun x ->"
          , text $ qualify showM "show"
          , parens $ text (showsFunQual (qualify showM) cat) <+> "x"
          ]
        topType     = text (fixTypeQual absM $ normCat cat)
    in vcat
        [ "open Lexing"
        , ""
        , "let parse (c : in_channel) :" <+> topType <+> "="
        , nest 4 (parserName <+> lexerName <+> "(Lexing.from_channel c)")
        , ";;"
        , ""
        , "let showTree (t : " <> topType <> ") : string ="
        , nest 4 (fsep ( punctuate "^"
            [ doubleQuotes "[Abstract syntax]\\n\\n"
            , showFun <+> "t"
            , doubleQuotes "\\n\\n"
            , doubleQuotes "[Linearized tree]\\n\\n"
            , printerName <+> "t"
            , doubleQuotes "\\n" ] ) )
        , ";;"
        , ""
        , "let main () ="
        , nest 4 $ vcat
            [ "let channel ="
            , nest 4 $ vcat
                [ "if Array.length Sys.argv > 1 then open_in Sys.argv.(1)"
                , "else stdin" ]
            , "in"
            , "try"
            , nest 4 $ vcat
                [ "print_string (showTree (parse channel));"
                , "flush stdout;"
                , "exit 0"]
            , "with BNFC_Util.Parse_error (start_pos, end_pos) ->"
            , nest 4 $ vcat
                [ "Printf.printf \"Parse error at %d.%d-%d.%d\\n\""
                , nest 4 $ vcat
                    [ "start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)"
                    , "end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol);" ]
                , "exit 1" ]]
        , ";;"
        , ""
        , "main ();;" ]
