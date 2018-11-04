{-
    BNF Converter: C++ Skeleton generation
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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the C++ Skeleton functions.

                    The generated files use the Visitor design pattern.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 9 August, 2003

    Modified      : 29 August, 2006 Aarne Ranta


   **************************************************************
-}

module BNFC.Backend.CPP.STL.CFtoCVisitSkelSTL (cf2CVisitSkel) where

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common.OOAbstract
import BNFC.Backend.CPP.Naming
import BNFC.Backend.CPP.STL.STLUtils

--Produces (.H file, .C file)
cf2CVisitSkel :: Maybe String -> CF -> (String, String)
cf2CVisitSkel inPackage cf = (mkHFile inPackage cab, mkCFile inPackage cab)
 where
    cab = cf2cabs cf

-- **** Header (.H) File Functions ****

--Generates the Header File
mkHFile :: Maybe String -> CAbs -> String
mkHFile inPackage cf = unlines [
  "#ifndef " ++ hdef,
  "#define " ++ hdef,
  "/* You might want to change the above name. */",
  "",
  "#include \"Absyn.H\"",
  "",
  nsStart inPackage,
  "class Skeleton : public Visitor",
  "{",
  "public:",
  unlines ["  void visit" ++ b ++ "(" ++ b ++ " *p);" |
                              b <- classes, notElem b (defineds cf)],
  unlines ["  void visit" ++ b ++ "(" ++ b ++  " x);" | b <- basics],
  "};",
  nsEnd inPackage,
  "",
  "#endif"
 ]
 where
   hdef = nsDefine inPackage "SKELETON_HEADER"
   classes = allClasses cf
   basics = tokentypes cf ++ map fst basetypes


-- **** Implementation (.C) File Functions ****

--Makes the .C File
mkCFile :: Maybe String -> CAbs -> String
mkCFile inPackage cf = unlines [
  headerC,
  nsStart inPackage,
  unlines [
    "void Skeleton::visit" ++ t ++ "(" ++
       t ++ " *t) {} //abstract class" | t <- absclasses cf],
  unlines [prCon   r  | (_,rs)  <- signatures cf, r <- rs],
  unlines [prList  cb | cb <- listtypes cf],
  unlines [prBasic b  | b  <- tokentypes cf ++ map fst basetypes],
  nsEnd inPackage
 ]

headerC = unlines [
      "/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/",
      "/* This implements the common visitor design pattern.",
      "   Note that this method uses Visitor-traversal of lists, so",
      "   List->accept() does NOT traverse the list. This allows different",
      "   algorithms to use context information differently. */",
      "",
      "#include \"Skeleton.H\"",
      ""
      ]

prBasic c = unlines [
  "void Skeleton::visit" ++ c ++ "(" ++ c ++ " x)",
  "{",
  "  /* Code for " ++ c ++ " Goes Here */",
  "}"
  ]

prList (cl,b) = unlines [
  "void Skeleton::visit" ++ cl ++ "("++ cl +++ "*" ++ vname ++ ")",
  "{",
  "  for ("++ cl ++"::iterator i = " ++
       vname++"->begin() ; i != " ++vname ++"->end() ; ++i)",
  "  {",
  if b
    then "    (*i)->accept(this);"
    else "    visit" ++ drop 4 cl ++ "(*i) ;",
  "  }",
  "}"
  ]
 where
   vname = mkVariable cl

prCon (f,cs) = unlines [
  "void Skeleton::visit" ++ f ++ "(" ++ f ++ " *" ++ v ++ ")",
  "{",
  "  /* Code For " ++ f ++ " Goes Here */",
  "",
  unlines ["  " ++ visitArg c | c <- cs],
  "}"
 ]
 where
   v = mkVariable f
   visitArg (cat,isPt,var) =
     if isPt
       then (v ++ "->" ++ var ++ "->accept(this);")
       else ("visit" ++ cat ++ "(" ++ v ++ "->" ++ var ++ ");")
