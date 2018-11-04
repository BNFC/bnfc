{-
    BNF Converter: C# Abstract Visit Skeleton Generator

    Copyright (C) 2006  Author:  Johan Broberg

    Modified from BNFC.Backend.CSharp.CAbstoCSharpVisitSkeleton

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

    Description   : This module generates an Abstract Visit Skeleton for C#.
                    This can be useful if you often make changes to your
                    grammar and want to keep your own changes (not having to
                    merge with the new visit skeleton each time), while still
                    getting compile errors if your code is no longer correct.
                    The generated file uses the Visitor design pattern.

                    This could have been generated from within
                    CAbstoVisitSkeleton, but that would have made it more
                    difficult to actually use it (and the visit skeleton).

    Author        : Johan Broberg (johan@pontemonti.com)

    License       : GPL (GNU General Public License)

    Created       : 19 December, 2006

    Modified      : 19 December, 2006 by Johan Broberg

   **************************************************************
-}

module BNFC.Backend.CSharp.CAbstoCSharpAbstractVisitSkeleton (cabs2csharpAbstractVisitSkeleton) where

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common.OOAbstract hiding (basetypes)
import BNFC.Backend.CSharp.CSharpUtils

--Produces .cs file
cabs2csharpAbstractVisitSkeleton :: Namespace -> CAbs -> String
cabs2csharpAbstractVisitSkeleton namespace cabs = unlines [
  "/*** BNFC-Generated Abstract Visitor Design Pattern Skeleton. ***/",
  "/* This implements the common visitor design pattern.",
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
  "  public abstract class Abstract" ++ c ++ "Visitor<R,A> : " ++ identifier namespace c ++ ".Visitor<R,A>",
  "  {",
  "    public abstract R Visit(" ++ identifier namespace (typename c) +++ varname c ++ ", A arg);",
  "  }"
  ]

prCon :: Namespace -> (String, [CAbsRule]) -> String
prCon namespace (c,fs) = unlinesInline [
  "  public abstract class Abstract" ++ c ++ "Visitor<R,A> : " ++ identifier namespace c ++ ".Visitor<R,A>",
  "  {",
  unlinesInlineMap (prVisit namespace) fs,
  "  }"
  ]

prVisit :: Namespace -> (Fun, [(String, Bool, String)]) -> String
prVisit namespace (f,_) = unlinesInline [
  "    public abstract R Visit(" ++ identifier namespace f +++ varname f ++ ", A arg);"
  ]
