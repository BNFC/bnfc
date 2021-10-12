{-# LANGUAGE TupleSections #-}

{-
    BNF Converter: C++ abstract syntax generator
    Copyright (C) 2004  Author:  Michael Pellauer

    Description   : This module generates the C++ Abstract Syntax
                    tree classes. It generates both a Header file
                    and an Implementation file, and uses the Visitor
                    design pattern. It uses STL (Standard Template Library).

    Author        : Michael Pellauer
    Created       : 4 August, 2003
    Modified      : 22 May, 2004 / Antti-Juhani Kaijanaho
                    29 August, 2006 / Aarne Ranta

-}

module BNFC.Backend.CPP.STL.CFtoSTLAbs (cf2CPPAbs) where

import Data.List        ( intercalate, intersperse )

import BNFC.Backend.Common.OOAbstract
import BNFC.CF
import BNFC.Options     ( RecordPositions(..) )
import BNFC.TypeChecker ( ListConstructors(..) )
import BNFC.Utils       ( (+++), applyWhen )

import BNFC.Backend.CPP.Common
import BNFC.Backend.CPP.STL.STLUtils

--The result is two files (.H file, .C file)

cf2CPPAbs :: RecordPositions -> Maybe String -> String -> CF -> (String, String)
cf2CPPAbs rp inPackage _ cf = (mkHFile rp inPackage cab cf, mkCFile inPackage cab cf)
  where
    cab = cf2cabs cf


-- **** Header (.H) File Functions **** --

--Makes the Header file.
mkHFile :: RecordPositions -> Maybe String -> CAbs -> CF -> String
mkHFile rp inPackage cabs cf = unlines
 [
  "#ifndef " ++ hdef,
  "#define " ++ hdef,
  "",
  "#include<string>",
  "#include<vector>",
  "",
  "//C++ Abstract Syntax Interface.",
  nsStart inPackage,
  "/********************   TypeDef Section    ********************/",
  "",
  unlines ["typedef " ++ d ++ " " ++ c ++ ";" | (c,d) <- basetypes],
  "",
  unlines ["typedef std::string " ++ s ++ ";" | s <- tokentypes cabs],
  "",
  "/********************   Forward Declarations    ********************/",
  "",
  unlines ["class " ++ c ++ ";" | c <- classes, notElem c (defineds cabs)],
  "",
  "/********************   Visitor Interfaces    ********************/",
  prVisitor cabs,
  "",
  prVisitable,
  "",
  "/********************   Abstract Syntax Classes    ********************/",
  "",
  unlines [prAbs rp c | c <- absclasses cabs],
  "",
  unlines [prCon (c,r) | (c,rs) <- signatures cabs, r <- rs],
  "",
  unlines [prList c | c <- listtypes cabs],
  "",
  definedRules Nothing cf
  "/********************   Defined Constructors    ********************/",
  nsEnd inPackage,
  "#endif"
 ]
 where
  classes = allClasses cabs
  hdef = nsDefine inPackage "ABSYN_HEADER"

-- auxiliaries

prVisitable :: String
prVisitable = unlines [
  "class Visitable",
  "{",
  " public:",
  -- all classes with virtual methods require a virtual destructor
  "  virtual ~Visitable() {}",
  "  virtual void accept(Visitor *v) = 0;",
  "};"
  ]

prVisitor :: CAbs -> String
prVisitor cf = unlines [
  "class Visitor",
  "{",
  "public:",
  "  virtual ~Visitor() {}",
  unlines
    ["  virtual void visit"++c++"("++c++" *p) = 0;" | c <- allClasses cf,
                                                      notElem c (defineds cf)],
  "",
  unlines
    ["  virtual void visit"++c++"(" ++c++" x) = 0;" | c <- allNonClasses cf],
  "};"
 ]

prAbs :: RecordPositions -> String -> String
prAbs rp c = unlines [
  "class " ++ c ++ " : public Visitable",
  "{",
  "public:",
  "  virtual " ++ c ++ " *clone() const = 0;",
  if rp == RecordPositions then "  int line_number, char_number;" else "",
  "};"
  ]

prCon :: (String, CAbsRule) -> String
prCon (c,(f,cs)) = unlines [
  "class " ++f++ " : public " ++ c,
  "{",
  "public:",
  unlines
    ["  "++ typ +++ pointerIf st var ++ ";" | (typ,st,var) <- cs],
  "  " ++ f ++ "(const " ++ f ++ " &);",
  "  " ++ f ++ " &operator=(const " ++f++ " &);",
  "  " ++ f ++ "(" ++ conargs ++ ");",
    -- Typ *p1, PIdent *p2, ListStm *p3);
  "  ~" ++f ++ "();",
  "  virtual void accept(Visitor *v);",
  "  virtual " ++f++ " *clone() const;",
  "  void swap(" ++f++ " &);",
  "};"
  ]
 where
   conargs = concat $ intersperse ", "
     [x +++ pointerIf st ("p" ++ show i) | ((x,st,_),i) <- zip cs [1..]]

prList :: (String, Bool) -> String
prList (c, b) = unlines
  [ "class " ++c++ " : public Visitable, public std::vector<" ++bas++ ">"
  , "{"
  , "public:"
  , "  virtual void accept(Visitor *v);"
  , "  virtual " ++ c ++ " *clone() const;"
  , "};"
  , ""
    -- cons for this list type
  , concat [ c, "* ", "cons", c, "(", bas, " x, ", c, "* xs);" ]
  ]
  where
  bas = applyWhen b (++ "*") $ drop 4 c {- drop "List" -}


-- **** Implementation (.C) File Functions **** --

mkCFile :: Maybe String -> CAbs -> CF -> String
mkCFile inPackage cabs cf = unlines $ [
  "//C++ Abstract Syntax Implementation.",
  "#include <algorithm>",
  "#include <string>",
  "#include <vector>",
  "#include \"Absyn.H\"",
  nsStart inPackage,
  unlines [prConC  r | (_,rs) <- signatures cabs, r <- rs],
  unlines [prListC l | l <- listtypes cabs],
  definedRules (Just $ LC nil cons) cf
  "/********************   Defined Constructors    ********************/",
  nsEnd inPackage
  ]
  where
  nil  t = (,dummyType) $ concat [ "new List", identType t, "()" ]
  cons t = (,dummyType) $ concat [ "consList", identType t ]


prConC :: CAbsRule -> String
prConC fcs@(f,_) = unlines [
  "/********************   " ++ f ++ "    ********************/",
  prConstructorC fcs,
  prCopyC fcs,
  prDestructorC fcs,
  prAcceptC f,
  prCloneC f,
  ""
 ]

prListC :: (String,Bool) -> String
prListC (c,b) = unlines
  [ "/********************   " ++ c ++ "    ********************/"
  , ""
  , prAcceptC c
  , prCloneC c
  , prConsC c b
  ]


--The standard accept function for the Visitor pattern
prAcceptC :: String -> String
prAcceptC ty = unlines [
  "void " ++ ty ++ "::accept(Visitor *v)",
  "{",
  "  v->visit" ++ ty ++ "(this);",
  "}"
  ]

--The cloner makes a new deep copy of the object
prCloneC :: String -> String
prCloneC c = unlines [
  c +++ "*" ++ c ++ "::clone() const",
  "{",
  "  return new" +++ c ++ "(*this);",
  "}"
  ]

-- | Make a list constructor definition.
prConsC :: String -> Bool -> String
prConsC c b = unlines
  [ concat [ c, "* ", "cons", c, "(", bas, " x, ", c, "* xs) {" ]
  , "  xs->insert(xs->begin(), x);"
  , "  return xs;"
  , "}"
  ]
  where
  bas = applyWhen b (++ "*") $ drop 4 c {- drop "List" -}

--The constructor assigns the parameters to the corresponding instance variables.
prConstructorC :: CAbsRule -> String
prConstructorC (f,cs) = unlines [
  f ++ "::" ++ f ++ "(" ++ conargs ++ ")",
  "{",
  unlines ["  " ++ c ++ " = " ++ p ++ ";" | (c,p) <- zip cvs pvs],
  "}"
  ]
 where
   cvs = [c | (_,_,c) <- cs]
   pvs = ['p' : show i | ((_,_,_),i) <- zip cs [1..]]
   conargs = intercalate ", "
     [x +++ pointerIf st v | ((x,st,_),v) <- zip cs pvs]


--Copy constructor and copy assignment
prCopyC :: CAbsRule -> String
prCopyC (c,cs) = unlines [
  c ++ "::" ++ c ++ "(const" +++ c +++ "& other)",
  "{",
  unlines ["  " ++ cv ++ " = other." ++ cloneIf st cv ++ ";" | (_,st,cv) <- cs],
  "}",
  "",
  c +++ "&" ++ c ++ "::" ++ "operator=(const" +++ c +++ "& other)",
  "{",
  "  " ++ c +++ "tmp(other);",
  "  swap(tmp);",
  "  return *this;",
  "}",
  "",
  "void" +++ c ++ "::swap(" ++ c +++ "& other)",
  "{",
  unlines ["  std::swap(" ++ cv ++ ", other." ++ cv ++ ");" | (_,_,cv) <- cs],
  "}"
  ]
 where
   cloneIf st cv = if st then (cv ++ "->clone()") else cv

--The destructor deletes all a class's members.
prDestructorC :: CAbsRule -> String
prDestructorC (c,cs) = unlines [
  c ++ "::~" ++ c ++"()",
  "{",
  unlines ["  delete(" ++ cv ++ ");" | (_,isPointer,cv) <- cs, isPointer],
  "}"
  ]
