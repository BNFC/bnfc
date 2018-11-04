{-
    BNF Converter: C# Visit Skeleton Generator

    Copyright (C) 2006  Author:  Johan Broberg

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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the C# Visit Skeleton.
                    The generated file uses the Visitor design pattern.

    Author        : Johan Broberg (johan@pontemonti.com)

    License       : GPL (GNU General Public License)

    Created       : 30 November, 2006

    Modified      : 21 January, 2007 by Johan Broberg

   **************************************************************
-}

module BNFC.Backend.CSharp.CAbstoCSharpVisitSkeleton (cabs2csharpvisitskeleton) where

import BNFC.Utils ((+++))
import Data.List
import BNFC.Backend.Common.OOAbstract hiding (basetypes)
import BNFC.Backend.CSharp.CSharpUtils

--Produces .cs file
cabs2csharpvisitskeleton :: Namespace -> CAbs -> String
cabs2csharpvisitskeleton namespace cabs = unlines [
  "/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/",
  "/* This implements the common visitor design pattern. To make sure that",
  "   compile errors occur when code in the Visitor don't match the abstract",
  "   syntaxt, the \"abstract visit skeleton\" is used.",
  "   ",
  "   Replace the R and A parameters with the desired return",
  "   and context types.*/",
  "",
  "namespace " ++ namespace ++ ".VisitSkeleton",
  "{",
  "  #region Classes",
  unlinesInlineMap (prCon   namespace) (signatures cabs),
  "  #endregion",
  "  ",
  "  #region Token types",
  unlinesInlineMap (prBasic namespace) (tokentypes cabs),
  "  #endregion",
  "}"
  ]

prBasic namespace c = unlinesInline [
  "  public class " ++ c ++ "Visitor<R,A> : Abstract" ++ c ++ "Visitor<R,A>",
  "  {",
  "    public override R Visit(" ++ identifier namespace (typename c) +++ varname c ++ ", A arg)",
  "    {",
  "      /* Code for " ++ c ++ " Goes Here */",
  "      return default(R);",
  "    }",
  "  }"
  ]

prCon :: Namespace -> (String, [CAbsRule]) -> String
prCon namespace (c,fs) = unlinesInline [
  "  public class " ++ c ++ "Visitor<R,A> : Abstract" ++ c ++ "Visitor<R,A>",
  "  {",
  unlinesInlineMap (prVisit namespace) (map cabsrule2csharpabsrule fs),
  "  }"
  ]


prVisit :: Namespace -> CSharpAbsRule -> String
prVisit namespace (f,cs) = unlinesInline [
  "    public override R Visit(" ++ identifier namespace f +++ varname f ++ ", A arg)",
  "    {",
  "      /* Code For " ++ f ++ " Goes Here */",
  unlinesInline $ map (prVisitArg namespace (varname f)) cs,
  "      return default(R);",
  "    }"
  ]

prVisitArg :: Namespace -> String -> (String, Bool, VariableName, PropertyName) -> String
prVisitArg namespace vname (cat, _, var, prop)
  | cat `elem` (map fst basetypes)            = "      // " ++ vname ++ "." ++ prop
  -- var /= "list_" is a dummy fix to make sure that a category named "List" doesn't get interpreted as a List.
  -- this isn't very good though, and should be fixed somehow.
  | "list" `isPrefixOf` var && var /= "list_" = listAccept
  | otherwise                                 = "      " ++ vname ++ "." ++ prop ++ ".Accept(new " ++ cat ++ "Visitor<R,A>(), arg);"
  where
    listtype = typename (drop 4 cat)
    listAccept = unlinesInline [
      "      foreach(" ++ identifier namespace listtype ++ " x in " ++ vname ++ "." ++ prop ++ ")",
      "      {",
      if listtype `notElem` (map snd basetypes)
        then "        x.Accept(new " ++ listtype ++ "Visitor<R,A>(), arg);"
        else "        // x",
      "      }"
      ]
