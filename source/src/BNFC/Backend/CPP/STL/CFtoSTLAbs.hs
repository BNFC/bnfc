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
                    22 October, 2021 / Hiroyuki Nagata

-}

module BNFC.Backend.CPP.STL.CFtoSTLAbs ( cf2CPPAbs, CppStdMode(..) ) where

import Data.List        ( intercalate, intersperse )
import Data.Char        ( toLower )

import BNFC.Backend.Common.OOAbstract
import BNFC.CF
import BNFC.Options     ( RecordPositions(..) )
import BNFC.TypeChecker ( ListConstructors(..) )
import BNFC.Utils       ( (+++), applyWhen )

import BNFC.Backend.CPP.Common
import BNFC.Backend.CPP.STL.STLUtils


--The result is two files (.H file, .C file)

cf2CPPAbs :: RecordPositions -> CppStdMode -> Maybe String -> String -> CF -> (String, String)
cf2CPPAbs rp mode inPackage _ cf = (mkHFile rp mode inPackage cabs cf, mkCFile mode inPackage cabs cf)
  where
    cabs = cf2cabs cf


-- **** Header (.H) File Functions **** --

--Makes the Header file.
mkHFile :: RecordPositions -> CppStdMode -> Maybe String -> CAbs -> CF -> String
mkHFile rp mode inPackage cabs cf = unlines
 [
  "#ifndef " ++ hdef,
  "#define " ++ hdef,
  "",
  case mode of {
    CppStdAnsi _ -> unlines [
        "#include <string>",
        "#include <vector>",
        "#include <algorithm>"];
    CppStdBeyondAnsi _ -> unlines [
        "#include <string>",
        "#include <list>",
        "#include <algorithm>",
        "#include <memory>"];
    },
  "//C++ Abstract Syntax Interface.",
  nsStart inPackage,
  "/********************   TypeDef Section    ********************/",
  "",
  case mode of {
    CppStdAnsi _ -> unlines $
      ["typedef " ++ d ++ " " ++ c ++ ";" | (c,d) <- basetypes]
      ++ [" "]
      ++ ["typedef std::string " ++ s ++ ";" | s <- tokentypes cabs]
      ++ [" "];
    ;
    -- use "using" statement
    CppStdBeyondAnsi _ -> unlines $
      ["using " ++ c ++ " = " ++ d ++ ";" | (c,d) <- basetypes]
      ++ [""]
      ++ ["using " ++ s ++ " = std::string;" | s <- tokentypes cabs]
      ++ [""];
    },
  "/********************   Forward Declarations    ********************/",
  "",
  case mode of {
    CppStdAnsi _ -> unlines $
      ["class " ++ c ++ ";" | c <- classes, notElem c (defineds cabs)]
    ;
    CppStdBeyondAnsi _ -> unlines $
      ["class " ++ c ++ ";" | c <- classes, notElem c (defineds cabs)]
    ;
    },
  "",
  "/********************   Visitor Interfaces    ********************/",
  prVisitor cabs,
  "",
  prVisitable,
  "",
  "/********************   Abstract Syntax Classes    ********************/",
  "",
  unlines [prAbs mode rp c | c <- absclasses cabs],
  "",
  unlines [prCon mode (c,r) | (c,rs) <- signatures cabs, r <- rs],
  "",
  unlines [prList mode primitives c | c <- listtypes cabs],
  "",
  definedRules mode Nothing cf
  "/********************   Defined Constructors    ********************/",
  nsEnd inPackage,
  "#endif"
 ]
 where
  classes = allClasses cabs
  hdef = nsDefine inPackage "ABSYN_HEADER"
  primitives = [c | (c,_) <- basetypes] ++ tokentypes cabs

-- auxiliaries

prVisitable :: String
prVisitable = unlines [
  "class Visitable",
  "{",
  " public:",
  -- all classes with virtual methods require a virtual destructor
  "    virtual ~Visitable() {}",
  "    virtual void accept(Visitor *v) = 0;",
  "};"
  ]

prVisitor :: CAbs -> String
prVisitor cf = unlines [
  "class Visitor",
  "{",
  "public:",
  "    virtual ~Visitor() {}",
  unlines
  ["    virtual void visit"++c++"("++ c +++ vararg ++") = 0;" | c <- allClasses cf, notElem c (defineds cf)],
  "",
  unlines
  ["    virtual void visit"++c++"("++c++" x) = 0;" | c <- allNonClasses cf],
  "};"
  ]
  where
    vararg = "*p"

prAbs :: CppStdMode -> RecordPositions -> String -> String
prAbs mode rp c =
  case mode of {
    CppStdAnsi _ -> unlines [
        "class " ++ c ++ " : public Visitable",
          "{",
          "public:",
          "    virtual " ++ c ++ " *clone() const = 0;",
          if rp == RecordPositions then "    int line_number, char_number;" else "",
          "};"
        ];
    CppStdBeyondAnsi _ -> unlines [
        "class " ++ c ++ " : public Visitable",
          "{",
          "public:",
          "    virtual" +++ wrapSharedPtr c +++ "clone() const = 0;",
          if rp == RecordPositions then "    int line_number, char_number;" else "",
          "};"
        ];
    }

prCon :: CppStdMode -> (String, CAbsRule) -> String
prCon mode (c,(f,cs)) =
  case mode of {
    CppStdAnsi _ -> unlines [
        "class " ++f++ " : public " ++ c,
        "{",
        "public:",
        unlines
        ["    "++ typ +++ pointerIf st var ++ ";" | (typ,st,var) <- cs],
        "    " ++ f ++ "(const " ++ f ++ " &);",
        "    " ++ f ++ " &operator=(const " ++f++ " &);",
        "    " ++ f ++ "(" ++ conargs ++ ");",
        -- Typ *p1, PIdent *p2, ListStm *p3);
        "    ~" ++f ++ "();",
        "    virtual void accept(Visitor *v);",
        "    virtual " ++f++ " *clone() const;",
        "    void swap(" ++f++ " &);",
        "};"
        ];
    CppStdBeyondAnsi _ -> unlines [
        "class " ++f++ " : public " ++ c,
        "{",
        "public:",
        unlines ["    " ++ wrapSharedPtrIf isClass typ +++ var ++ ";" | (typ,isClass,var) <- cs],
        if length cs > 0 then
          -- Generate following initiliazer;
          --
          -- Prog(std::shared_ptr<ListStatement> p1)
          --     : Program(), liststatement_{p1} {};
          unlines
          [ "    " ++f++ "(" ++ conargs ++ ")",
            "    :" +++ c ++ "(), " ++ intercalate ", " [var ++ "{p" ++ show i ++ "}" | ((_,_,var),i) <- zip cs [(1::Integer)..]],
            "    {};"
          ]
        else
          "    " ++f++ "(" ++ conargs ++ "):" +++ c +++ "(){};",
        "",
        "    virtual void accept(Visitor *v) override;",
        "    " ++ wrapSharedPtr c +++ " clone() const;",
        "};"
        ];
      }
 where
   conargs =
     case mode of {
       CppStdAnsi _ ->
           concat $ intersperse ", "
           [x +++ pointerIf st ("p" ++ show i) | ((x,st,_),i) <- zip cs [(1::Integer)..]]
       ;
       CppStdBeyondAnsi _ ->
           intercalate ", " [wrapSharedPtrIf isClass x ++ " p" ++ show i | ((x,isClass,_),i) <- zip cs [(1::Integer)..]]
       ;
       }

prList :: CppStdMode -> [String] -> (String, Bool) -> String
prList mode primitives (c, b) = case mode of {
  CppStdAnsi _ -> unlines [
      "class " ++c++ " : public Visitable, public std::vector<" ++bas++ ">"
      , "{"
      , "public:"
      , "    virtual void accept(Visitor *v);"
      , "    virtual " ++ c ++ " *clone() const;"
      , "};"
      , ""
      -- cons for this list type
      , concat [ c, "* ", "cons", c, "(", bas, " x, ", c, "* xs);" ]
      ];
  CppStdBeyondAnsi _ -> unlines [
      "class " ++c++ " : public Visitable"
      , "{"
      , "public:"
      , "    " ++c++ "() : " ++childClassVarName++ "{}"
      , "    {}"
      , ""
      , "    std::list<" ++ wrapSharedPtr childClass++ ">" +++ childClassVarName ++ ";"
      , ""
      -- ref: https://stackoverflow.com/questions/51148797/how-can-i-define-iterator-and-const-iterator-in-my-class-while-i-uses-stdvecto
      , "    // define iterator and const_iterator, expose it"
      , "    using iterator = typename std::list<" ++ wrapSharedPtr childClass ++ ">::iterator;"
      , "    using const_iterator = typename std::list<" ++ wrapSharedPtr childClass++ ">::const_iterator;"
      , "    auto begin() const { return " ++childClassVarName++ ".begin(); }"
      , "    auto begin()       { return " ++childClassVarName++ ".begin(); }"
      , "    auto end()   const { return " ++childClassVarName++ ".end(); }"
      , "    auto end()         { return " ++childClassVarName++ ".end(); }"
      , ""
      , "    virtual void accept(Visitor *v);"
      , "    " ++ wrapSharedPtr c +++ " clone() const;"
      , "    void cons(" ++ wrapSharedPtrIf isNotBaseClass childClass ++ ");"
      , "    void reverse();"
      , "};"
      , ""
      ];
    }
  where
    childClass = drop 4 c
    childClassVarName = "list" ++ map toLower childClass ++ "_"
    bas = applyWhen b (++ "*") $ drop 4 c {- drop "List" -}
    -- if list element is primitive type, not to use smart-ptr for argument type
    isNotBaseClass = not $ elem childClass primitives


-- **** Implementation (.C) File Functions **** --

mkCFile :: CppStdMode -> Maybe String -> CAbs -> CF -> String
mkCFile mode inPackage cabs cf = unlines $ [
  "//C++ Abstract Syntax Implementation.",
  "#include <algorithm>",
  "#include <string>",
  "#include <vector>",
  "#include \"Absyn"++hExt++"\"",
  nsStart inPackage,
  unlines [prConC  mode c r  | (c,rs) <- signatures cabs, r <- rs],
  unlines [prListC mode primitives l | l <- listtypes cabs],
  definedRules mode (Just $ LC nil cons) cf
  "/********************   Defined Constructors    ********************/",
  nsEnd inPackage
  ]
  where
    primitives = [c | (c,_) <- basetypes] ++ tokentypes cabs
    nil  t = case mode of
      CppStdAnsi _ -> (,dummyType) $ concat [ "new List", identType t, "()" ]
      CppStdBeyondAnsi _ -> (,dummyType) $ wrapMakeShared ("List" ++ identType t) ++ "()"
    cons t = case mode of
      CppStdAnsi _ -> (,dummyType) $ concat [ "consList", identType t ]
      CppStdBeyondAnsi _ -> (,dummyType) $ concat [ "consList", identType t ]
    hExt = case mode of
      CppStdAnsi _ -> ".h"
      CppStdBeyondAnsi _ -> ".hh"


prConC :: CppStdMode -> String -> CAbsRule -> String
prConC mode c fcs@(f,_) = unlines [
  "/********************   " ++ f ++ "    ********************/",
  prConstructorC mode fcs,
  prCopyC mode fcs,
  prDestructorC mode fcs,
  prAcceptC mode f,
  prCloneC mode c f,
  ""
 ]

prListC :: CppStdMode -> [String] -> (String,Bool) -> String
prListC mode primitives (c,b) = unlines
  [ "/********************   " ++ c ++ "    ********************/"
  , prAcceptC mode c
  , prCloneC mode c c
  , prConsC mode primitives c b
  ]

--The standard accept function for the Visitor pattern
prAcceptC :: CppStdMode -> String -> String
prAcceptC mode ty = case mode of {
    CppStdAnsi _ -> unlines [
        "void " ++ ty ++ "::accept(Visitor *v)",
        "{",
        "    v->visit" ++ ty ++ "(this);",
        "}"
        ];
    CppStdBeyondAnsi _ -> unlines [
        "void " ++ty++ "::accept(Visitor *v)",
        "{",
        "    v->visit" ++ ty ++ "(this);",
        "}"
        ];
    }

--The cloner makes a new deep copy of the object
prCloneC :: CppStdMode -> String -> String -> String
prCloneC mode f c = case mode of {
  CppStdAnsi _ -> unlines [
      c +++ "*" ++ c ++ "::clone() const",
      "{",
      "    return new" +++ c ++ "(*this);",
      "}"
      ];
  CppStdBeyondAnsi _ -> unlines [
      wrapSharedPtr f +++ c ++ "::clone() const ",
      "{",
      "    return std::make_shared<" ++ c ++ ">(*this);",
      "}"
      ];
  }

-- | Make a list constructor definition.
prConsC :: CppStdMode -> [String] -> String -> Bool -> String
prConsC mode primitives c b = case mode of {
    CppStdAnsi _ -> unlines [
        concat [ c, "* ", "cons", c, "(", bas, " x, ", c, "* xs) {" ]
        , "    xs->insert(xs->begin(), x);"
        , "    return xs;"
        , "}"
        ];
    CppStdBeyondAnsi _ -> unlines [
        -- Append a element into list tail (In C ++ term, "push_back")
        concat [ "void ", c, "::cons(", consArg, " x) {" ]
        , if isNotBaseClass then
            "    " ++inner++ ".push_back(x);"
          else
            "    " ++inner++ ".push_back(std::make_unique<" ++bas++ ">(x));"
        , "}"
        , ""
        -- Insert a element into list head (In C ++ term, "push_front" / in lisp term ? "cons")
        -- This implementation is required in definedRules
        , concat [wrapSharedPtr c, " cons", c, "(", consArg, " x, ", wrapSharedPtr c, " xs) {"]
        , if isNotBaseClass then
            "    xs->" ++inner++ ".push_front(x);"
          else
            "    xs->" ++inner++ ".push_front(std::make_unique<" ++bas++ ">(x));"
        , "    return xs;"
        , "}"
        , ""
        , "void" +++ c ++ "::reverse() {"
        , "    std::reverse(" ++inner++ ".begin(), " ++inner++ ".end());"
        , "}"
        ];
      }
  where
    bas = case mode of {
      CppStdAnsi _ -> applyWhen b (++ "*") $ drop 4 c {- drop "List" -};
      CppStdBeyondAnsi _ -> drop 4 c;
      }
    inner = map toLower c ++ "_"
    -- if list element is primitive type, not to use smart-ptr for argument type
    isNotBaseClass = not $ elem bas primitives
    consArg = wrapSharedPtrIf isNotBaseClass bas


--The constructor assigns the parameters to the corresponding instance variables.
prConstructorC :: CppStdMode -> CAbsRule -> String
prConstructorC mode (f,cs) = case mode of {
  CppStdAnsi _ -> unlines [
      f ++ "::" ++ f ++ "(" ++ conargs ++ ")",
      "{",
      unlines ["    " ++ c ++ " = " ++ p ++ ";" | (c,p) <- zip cvs pvs],
      "}"
      ];
  CppStdBeyondAnsi _ -> unlines [
      ];
    }
 where
   cvs = [c | (_,_,c) <- cs]
   pvs = ['p' : show i | ((_,_,_),i) <- zip cs [(1::Integer)..]]

   conargs = case mode of {
     CppStdAnsi _ ->
         intercalate ", " [x +++ pointerIf isClass v | ((x,isClass,_),v) <- zip cs pvs]
     ;
     CppStdBeyondAnsi _ ->
         ""
     ;
     }


--Copy constructor and copy assignment
prCopyC :: CppStdMode -> CAbsRule -> String
prCopyC mode (c,cs) = case mode of {
  CppStdAnsi _ -> unlines [
      c ++ "::" ++ c ++ "(const" +++ c +++ "& other)",
      "{",
      unlines ["    " ++ cv ++ " = other." ++ cloneIf st cv ++ ";" | (_,st,cv) <- cs],
      "}",
      "",
      c +++ "&" ++ c ++ "::" ++ "operator=(const" +++ c +++ "& other)",
      "{",
      "    " ++ c +++ "tmp(other);",
      "    swap(tmp);",
      "    return *this;",
      "}",
      "",
      "void" +++ c ++ "::swap(" ++ c +++ "& other)",
      "{",
      unlines ["    std::swap(" ++ cv ++ ", other." ++ cv ++ ");" | (_,_,cv) <- cs],
      "}"
      ];
  CppStdBeyondAnsi _ -> ""
  }
  where
    cloneIf st cv = if st then (cv ++ "->clone()") else cv

--The destructor deletes all a class's members.
prDestructorC :: CppStdMode -> CAbsRule -> String
prDestructorC mode (c,cs) = case mode of {
  CppStdAnsi _ -> unlines [
      c ++ "::~" ++ c ++"()",
      "{",
      unlines ["    delete(" ++ cv ++ ");" | (_,isPointer,cv) <- cs, isPointer],
      "}"
      ];
  CppStdBeyondAnsi _ -> ""
  ;
  }
