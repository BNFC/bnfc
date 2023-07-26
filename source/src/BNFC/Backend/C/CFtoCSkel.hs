{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: C Skeleton generator
    Copyright (C) 2004  Author:  Michael Pellauer

    Description   : This module generates the C Skeleton functions.

                    The generated files follow Appel's case method.

    Author        : Michael Pellauer
    Created       : 9 August, 2003
-}

module BNFC.Backend.C.CFtoCSkel (cf2CSkel) where

import Prelude hiding ((<>))

import BNFC.CF
import BNFC.Utils                         ( (+++), capitalize )
import BNFC.Backend.Common.NamedVariables
import Data.Char                          ( toLower )
import Data.Either                        ( lefts )

import Text.PrettyPrint

--Produces (.H file, .C file)
cf2CSkel :: CF -> (String, String)
cf2CSkel cf = (mkHFile cf groups, mkCFile cf groups)
 where
    groups = fixCoercions (ruleGroupsInternals cf)


{- **** Header (.H) File Functions **** -}

--Generates the Header File
mkHFile :: CF -> [(Cat,[Rule])] -> String
mkHFile cf groups = unlines
 [
  header,
  concatMap prDataH groups,
  concatMap prUserH user,
  footer
 ]
 where
  user = map fst $ tokenPragmas cf
  header = unlines
   [
    "#ifndef SKELETON_HEADER",
    "#define SKELETON_HEADER",
    "/* You might want to change the above name. */",
    "",
    "#include \"Absyn.h\"",
    ""
   ]
  prUserH u = "void visit" ++ basicFunNameS u ++ "(" ++ u ++ " p);"
  footer = unlines
   [
    "void visitIdent(Ident i);",
    "void visitInteger(Integer i);",
    "void visitDouble(Double d);",
    "void visitChar(Char c);",
    "void visitString(String s);",
    "",
    "#endif"
   ]

--Prints out visit functions for a category
prDataH :: (Cat, [Rule]) -> String
prDataH (cat, _rules) =
    if isList cat
      then concat ["void visit", cl, "(", cl,  " p);\n"]
      else "void visit" ++ cl ++ "(" ++ cl ++ " p);\n"
    where cl = identCat $ normCat cat

{- **** Implementation (.C) File Functions **** -}

-- | Makes the skeleton's .c File
mkCFile :: CF -> [(Cat,[Rule])] -> String
mkCFile cf groups = concat
  [ header
  , concatMap prData groups
  , concatMap prUser user
  , footer
  ]
  where
    user = map fst $ tokenPragmas cf
    header = unlines [
      "/*** Visitor Traversal Skeleton. ***/",
      "/* This traverses the abstract syntax tree.",
      "   To use, copy Skeleton.h and Skeleton.c to",
      "   new files. */",
      "",
      "#include <stdlib.h>",
      "#include <stdio.h>",
      "",
      "#include \"Skeleton.h\"",
      ""
      ]
    prUser u = unlines
     [
      "void visit" ++ basicFunNameS u ++ "(" ++ u ++ " p)",
      "{",
      "  /* Code for " ++ u ++ " Goes Here */",
      "}"
     ]
    footer = unlines
     [
      "void visitIdent(Ident i)",
      "{",
      "  /* Code for Ident Goes Here */",
      "}",
      "void visitInteger(Integer i)",
      "{",
      "  /* Code for Integer Goes Here */",
      "}",
      "void visitDouble(Double d)",
      "{",
      "  /* Code for Double Goes Here */",
      "}",
      "void visitChar(Char c)",
      "{",
      "  /* Code for Char Goes Here */",
      "}",
      "void visitString(String s)",
      "{",
      "  /* Code for String Goes Here */",
      "}",
      ""
     ]

--Visit functions for a category.
prData :: (Cat, [Rule]) -> String
prData (cat, rules)
  | isList cat = unlines
               [
                "void visit" ++ cl ++ "("++ cl +++ vname ++ ")",
                "{",
                "  while(" ++ vname +++ " != 0)",
                "  {",
                "    /* Code For " ++ cl ++ " Goes Here */",
                "    visit" ++ ecl ++ "(" ++ vname ++ "->" ++ member ++ "_);",
                "    " ++ vname +++ "=" +++ vname ++ "->" ++ vname ++ "_;",
                "  }",
                "}",
                ""
               ]
      -- Not a list:
  | otherwise = unlines
               [
                "void visit" ++ cl ++ "(" ++ cl ++ " p)",
                "{",
                "  switch(p->kind)",
                "  {",
                concatMap (render . prPrintRule) rules,
                "  default:",
                "    fprintf(stderr, \"Error: bad kind field when printing " ++ cl ++ "!\\n\");",
                "    exit(1);",
                "  }",
                "}\n"
               ]
    where cl = identCat $ normCat cat
          ecl = identCat $ normCatOfList cat
          vname = map toLower cl
          member = map toLower ecl

-- | Visits all the instance variables of a category.
-- >>> let ab = Cat "Ab"
-- >>> prPrintRule (Rule "Abc" undefined [Left ab, Left ab] Parsable)
--   case is_Abc:
--     /* Code for Abc Goes Here */
--     visitAb(p->u.abc_.ab_1);
--     visitAb(p->u.abc_.ab_2);
--     break;
-- <BLANKLINE>
-- >>> let ab = TokenCat "Ab"
-- >>> prPrintRule (Rule "Abc" undefined [Left ab] Parsable)
--   case is_Abc:
--     /* Code for Abc Goes Here */
--     visitAb(p->u.abc_.ab_);
--     break;
-- <BLANKLINE>
-- >>> prPrintRule (Rule "Abc" undefined [Left ab, Left ab] Parsable)
--   case is_Abc:
--     /* Code for Abc Goes Here */
--     visitAb(p->u.abc_.ab_1);
--     visitAb(p->u.abc_.ab_2);
--     break;
-- <BLANKLINE>
prPrintRule :: Rule -> Doc
prPrintRule (Rule f _c cats _)
  | isCoercion f    = empty
  | isDefinedRule f = empty
  | otherwise       = nest 2 $ vcat
    [ text $ "case is_" ++ fun ++ ":"
    , nest 2 (vcat
        [ "/* Code for " <> text fun <> " Goes Here */"
        , cats'
        , "break;\n"
        ])
    ]
  where
    fun = funName f
    cats' = vcat $ map (prCat fun) (lefts (numVars cats))

-- Prints the actual instance-variable visiting.
prCat :: Fun -> (Cat, Doc) -> Doc
prCat fnm (cat, vname) =
      let visitf = "visit" <> if isTokenCat cat
                       then basicFunName cat
                       else text (identCat (normCat cat))
      in visitf <> parens ("p->u." <> text v <> "_." <> vname ) <> ";"
    where v = map toLower fnm

-- | The visit-function name of a basic type

basicFunName :: Cat -> Doc
basicFunName = text . basicFunNameS . catToStr

basicFunNameS :: String -> String
basicFunNameS = capitalize
