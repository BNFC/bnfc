{-
    BNF Converter: C Skeleton generator
    Copyright (C) 2004  Author:  Michael Pellauer

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the C Skeleton functions.

                    The generated files follow Appel's case method.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 9 August, 2003

    Modified      : 12 August, 2003


   **************************************************************
-}

module BNFC.Backend.C.CFtoCSkel (cf2CSkel) where

import BNFC.CF
import BNFC.Utils                       ( (+++) )
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Utils (isTokenType)
import Data.Char                ( toLower, toUpper )
import Data.Either (lefts)

import Text.PrettyPrint

--Produces (.H file, .C file)
cf2CSkel :: CF -> (String, String)
cf2CSkel cf = (mkHFile cf groups, mkCFile cf groups)
 where
    groups = fixCoercions (ruleGroups cf)


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
  user = fst (unzip (tokenPragmas cf))
  header = unlines
   [
    "#ifndef SKELETON_HEADER",
    "#define SKELETON_HEADER",
    "/* You might want to change the above name. */",
    "",
    "#include \"Absyn.h\"",
    ""
   ]
  prUserH user = "void visit" ++ u' ++ "(" ++ show user ++ " p);"
    where
     u' = let u = show user in toUpper (head u) : map toLower (tail u) --this is a hack to fix a potential capitalization problem.
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
   [
    header,
    concatMap (prData user) groups,
    concatMap (prUser.show) user,
    footer
   ]
  where
    user = fst (unzip (tokenPragmas cf))
    header = unlines [
      "/*** BNFC-Generated Visitor Traversal Skeleton. ***/",
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
      "void visit" ++ u' ++ "(" ++ u ++ " p)",
      "{",
      "  /* Code for " ++ u ++ " Goes Here */",
      "}"
     ]
     where
      u' = toUpper (head u) : map toLower (tail u) --this is a hack to fix a potential capitalization problem.
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
prData :: [UserDef] -> (Cat, [Rule]) -> String
prData user (cat, rules)
  | isList cat = unlines
               [
                "void visit" ++ cl ++ "("++ cl +++ vname ++ ")",
                "{",
                "  while(" ++ vname ++ " != 0)",
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
                "void visit" ++ cl ++ "(" ++ cl ++ " _p_)",
                "{",
                "  switch(_p_->kind)",
                "  {",
                concatMap (render . prPrintRule user) rules,
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
-- >>> let ab = Cat "ab"
-- >>> prPrintRule [] (Rule "abc" undefined [Left ab, Left ab])
--   case is_abc:
--     /* Code for abc Goes Here */
--     visitab(_p_->u.abc_.ab_1);
--     visitab(_p_->u.abc_.ab_2);
--     break;
-- >>> prPrintRule [ab] (Rule "abc" undefined [Left ab])
--   case is_abc:
--     /* Code for abc Goes Here */
--     visitAb(_p_->u.abc_.ab_);
--     break;
-- >>> prPrintRule [ab] (Rule "abc" undefined [Left ab, Left ab])
--   case is_abc:
--     /* Code for abc Goes Here */
--     visitAb(_p_->u.abc_.ab_1);
--     visitAb(_p_->u.abc_.ab_2);
--     break;
prPrintRule :: [UserDef] -> Rule -> Doc
prPrintRule user (Rule fun _c cats) | not (isCoercion fun) = nest 2 $ vcat
    [ text $ "case is_" ++ fun ++ ":"
    , nest 2 (vcat
        [ "/* Code for " <> text fun <> " Goes Here */"
        , cats'
        , "break;" ])
    ]
  where
    cats' = vcat $ map (prCat user fun) (lefts (numVars cats))
prPrintRule _user (Rule _fun _ _) = ""

-- Prints the actual instance-variable visiting.
prCat :: [UserDef] -> Fun -> (Cat, Doc) -> Doc
prCat user fnm (cat, vname) =
      let visitf = "visit" <> if isTokenType user cat
                       then basicFunName cat
                       else text (identCat (normCat cat))
      in visitf <> parens ("_p_->u." <> text v <> "_." <> vname ) <> ";"
    where v = map toLower $ normFun fnm

--The visit-function name of a basic type
basicFunName :: Cat -> Doc
basicFunName c = text (toUpper (head (show c)): tail (show c))
