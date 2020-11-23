{-
    BNF Converter: C++ Skeleton generation
    Copyright (C) 2004  Author:  Michael Pellauer

    Description   : This module generates the C++ Skeleton functions.

                    The generated files use the Visitor design pattern.

    Author        : Michael Pellauer
    Created       : 9 August, 2003
    Modified      : 29 August, 2006 Aarne Ranta

-}

module BNFC.Backend.CPP.STL.CFtoCVisitSkelSTL (cf2CVisitSkel) where

import Data.Char

import BNFC.CF
import BNFC.Utils ((+++), unless)
import BNFC.Backend.Common.OOAbstract
import BNFC.Backend.CPP.Naming
import BNFC.Backend.CPP.STL.STLUtils

--Produces (.H file, .C file)
cf2CVisitSkel :: Bool -> Maybe String -> CF -> (String, String)
cf2CVisitSkel useSTL inPackage cf =
 ( mkHFile useSTL inPackage cab
 , mkCFile useSTL inPackage cab
 )
 where
    cab = cf2cabs cf

-- **** Header (.H) File Functions ****

--Generates the Header File
mkHFile :: Bool -> Maybe String -> CAbs -> String
mkHFile useSTL inPackage cf = unlines [
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
            b <- classes, notElem b (defineds cf), useSTL || notElem b (postokens cf) ],
  unlines ["  void visit" ++ b ++ "(" ++ b ++  " x);" | b <- basics useSTL cf ],
  "};",
  nsEnd inPackage,
  "",
  "#endif"
 ]
 where
   hdef = nsDefine inPackage "SKELETON_HEADER"
   classes = allClasses cf

-- CPP/NoSTL treats 'position token' as just 'token'.
basics :: Bool -> CAbs -> [String]
basics useSTL cf = concat
  [ map fst basetypes
  , tokentypes cf
  , unless useSTL $ postokens cf
  ]


-- **** Implementation (.C) File Functions ****

--Makes the .C File
mkCFile :: Bool -> Maybe String -> CAbs -> String
mkCFile useSTL inPackage cf = unlines [
  headerC,
  nsStart inPackage,
  unlines [
    "void Skeleton::visit" ++ t ++ "(" ++
       t ++ " *t) {} //abstract class" | t <- absclasses cf],
  unlines [ prCon   r  | (_,rs)  <- signatures cf, r <- rs, useSTL || not (posRule r) ],
  unlines [ prList useSTL cb | cb <- listtypes cf ],
  unlines [ prBasic b  | b  <- base ],
  nsEnd inPackage
 ]
  where
  -- See OOAbstract 'posdata':
  posRule (c, _) = c `elem` postokens cf
  base = basics useSTL cf
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
     visitArg (cat,isPt,var)
       | isPt && (useSTL || cat `notElem` base)
                   = "if (" ++ field ++ ") " ++ field ++ "->accept(this);"
       | otherwise = "visit" ++ cat ++ "(" ++ field ++ ");"
       where field = v ++ "->" ++ var

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

prList True (cl,b) = unlines [
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

prList False (cl,b) = unlines
  [ "void Skeleton::visit" ++ cl ++ "("++ cl +++ "*" ++ vname ++ ")"
  , "{"
  , "  while (" ++ vname ++ ")"
  , "  {"
  , "    /* Code For " ++ cl ++ " Goes Here */"
  , if b
      then "    if (" ++ field ++ ") " ++ field ++ "->accept(this);"
      else "    visit" ++ ecl ++ "(" ++ field ++ ");"
  , "    " ++ vname ++ " = " ++ vname ++ "->" ++ next ++ "_;"
  , "  }"
  , "}"
  ]
  where
  ecl    = drop 4 cl  -- drop "List"
  vname  = mkVariable cl
  next   = map toLower cl
  member = map toLower ecl ++ "_"
  field  = vname ++ "->" ++ member
