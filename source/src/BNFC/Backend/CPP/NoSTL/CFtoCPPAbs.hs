{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-
    BNF Converter: C++ abstract syntax generator
    Copyright (C) 2004  Author:  Michael Pellauer

    Description   : This module generates the C++ Abstract Syntax
                    tree classes. It generates both a Header file
                    and an Implementation file, and uses the Visitor
                    design pattern.

    Author        : Michael Pellauer
    Created       : 4 August, 2003
    Modified      : 22 May, 2004 / Antti-Juhani Kaijanaho
-}

module BNFC.Backend.CPP.NoSTL.CFtoCPPAbs (cf2CPPAbs) where

import Prelude hiding ((<>))

import Data.List  ( findIndices )
import Data.Char  ( toLower )
import Text.PrettyPrint

import BNFC.CF
import BNFC.TypeChecker ( ListConstructors(..) )
import BNFC.Utils       ( (+++), (++++) )

import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Common.OOAbstract
import BNFC.Backend.CPP.Common


--The result is two files (.H file, .C file)
cf2CPPAbs :: String -> CF -> (String, String)
cf2CPPAbs _ cf = (mkHFile cf, mkCFile cf)


{- **** Header (.H) File Functions **** -}

--Makes the Header file.
mkHFile :: CF -> String
mkHFile cf = unlines
 [
  "#ifndef ABSYN_HEADER",
  "#define ABSYN_HEADER",
  "",
  header,
  prTypeDefs user,
  "/********************   Forward Declarations    ********************/\n",
  concatMap prForward classes,
  "",
  prVisitor classes,
  prVisitable,
  "",
  "/********************   Abstract Syntax Classes    ********************/\n",
  concatMap (prDataH user) (getAbstractSyntax cf),
  "",
  definedRules Nothing cf
  "/********************   Defined Constructors    ********************/",
  "",
  "#endif"
 ]
 where
  user = fst (unzip (tokenPragmas cf)) -- includes position tokens
  -- user = [ name | TokenReg name False _ <- cfgPragmas cf ]  -- position tokens are in allClasses already
  header = "/* ~~~ C++ Abstract Syntax Interface.\n ~~~ */"
  ca = cf2cabs cf
  classes = absclasses ca ++ conclasses ca ++ map fst (listtypes ca)
  -- classes = allClasses (cf2cabs cf)  -- includes position tokens
  prForward s | isProperLabel s = "class " ++ s ++ ";\n"
  prForward _ = ""

--Prints interface classes for all categories.
prDataH :: [UserDef] -> Data -> String
prDataH  user (cat, rules) =
    case lookup (catToStr cat) rules of
        Just _ -> concatMap (prRuleH user cat) rules
        Nothing -> if isList cat
            then concatMap (prRuleH user cat) rules
            else unlines
                [ "class" +++ identCat cat +++ ": public Visitable {"
                , "public:"
                , "  virtual" +++ identCat cat +++ "*clone() const = 0;"
                , "};\n"
                , concatMap (prRuleH user cat) rules
                ]

--Interface definitions for rules vary on the type of rule.
prRuleH :: [UserDef] -> Cat -> (Fun, [Cat]) -> String
prRuleH user c (fun, cats) =
    if isNilFun fun || isOneFun fun
    then ""  --these are not represented in the AbSyn
    else if isConsFun fun
    then --this is the linked list case.
    unlines
    [
     "class" +++ c' +++ ": public Visitable",
     "{",
     " public:",
     render $ nest 2 $ prInstVars user vs,
     "  " ++ c' ++ "(const" +++ c' +++ "&);",
     "  " ++ c' ++ " &operator=(const" +++ c' +++ "&);",
     "  " ++ c' ++ "(" ++ (prConstructorH 1 vs) ++ ");",
     "  " ++ c' ++ "(" ++ mem +++ memstar ++ "p);",
     prDestructorH c',
     "  " ++ c' ++ "* reverse();",
     "  " ++ c' ++ "* reverse(" ++ c' ++ " *l);",
     "  virtual void accept(Visitor *v);",
     "  virtual " ++ c' ++ " *clone() const;",
     "  void swap(" ++ c' +++ "&);",
     "};"
    ]
    else --a standard rule
    unlines
    [
     "class" +++ fun +++ ": public" +++ super,
     "{",
     " public:",
     render $ nest 2 $ prInstVars user vs,
     "  " ++ fun ++ "(const" +++ fun +++ "&);",
     "  " ++ fun ++ " &operator=(const" +++ fun +++ "&);",
     "  " ++ fun ++ "(" ++ prConstructorH 1 vs ++ ");",
     prDestructorH fun,
     "  virtual void accept(Visitor *v);",
     "  virtual " +++ fun +++ " *clone() const;",
     "  void swap(" ++ fun +++ "&);",
     "};\n"
    ]
   where
     vs = getVars cats
     c' = identCat (normCat c);
     mem = drop 4 c'
     memstar = if isBasic user mem then "" else "*"
     super = if catToStr c == fun then "Visitable" else identCat c
     prConstructorH :: Int -> [(String, b)] -> String
     prConstructorH _ [] = ""
     prConstructorH n [(t,_)] = t +++ optstar t ++ "p" ++ show n
     prConstructorH n ((t,_):vs) = t +++ optstar t ++ "p" ++ show n ++ ", " ++ prConstructorH (n+1) vs
     prDestructorH n = "  ~" ++ n ++ "();"
     optstar x = if isBasic user x
       then ""
       else "*"

prVisitable :: String
prVisitable = unlines
 [
  "class Visitable",
  "{",
  " public:",
  -- all classes with virtual methods require a virtual destructor
  "  virtual ~Visitable() {}",
  "  virtual void accept(Visitor *v) = 0;",
  "};\n"
 ]

prVisitor :: [String] -> String
prVisitor fs = unlines
 [
  "/********************   Visitor Interfaces    ********************/",
  "",
  "class Visitor",
  "{",
  " public:",
  "  virtual ~Visitor() {}",
  concatMap prVisitFun fs,
  footer
 ]
 where
   footer = unlines
    [  --later only include used categories
     "  virtual void visitInteger(Integer i) = 0;",
     "  virtual void visitDouble(Double d) = 0;",
     "  virtual void visitChar(Char c) = 0;",
     "  virtual void visitString(String s) = 0;",
     "};"
    ]
   prVisitFun f | isProperLabel f =
     "  virtual void visit" ++ f ++ "(" ++ f ++ " *p) = 0;\n"
   prVisitFun _ = ""

--typedefs in the Header make generation much nicer.
prTypeDefs :: [String] -> String
prTypeDefs user = unlines
  [
   "/********************   TypeDef Section    ********************/",
   "typedef int Integer;",
   "typedef char Char;",
   "typedef double Double;",
   "typedef char* String;",
   "typedef char* Ident;",
   concatMap prUserDef user
  ]
 where
  prUserDef s = "typedef char* " ++ s ++ ";\n"

-- | A class's instance variables.
-- >>> prInstVars ["MyTokn"] [("MyTokn",1), ("A",1), ("A",2)]
-- MyTokn mytokn_1;
-- A *a_1, *a_2;
prInstVars :: [UserDef] -> [IVar] -> Doc
prInstVars _ [] = empty
prInstVars user vars@((t,_):_) =
    text t <+> uniques <> ";" $$ prInstVars user vs'
 where
    (uniques, vs') = prUniques t
    --these functions group the types together nicely
    prUniques :: String -> (Doc, [IVar])
    prUniques t = (prVars (findIndices (\(y,_) ->  y == t) vars), remType t vars)
    prVars = hsep . punctuate comma . map prVar
    prVar x = let (t,n) = vars !! x in varLinkName t <> text (showNum n)
    varLinkName z = if isBasic user z
      then text (map toLower z) <> "_"
      else "*" <> text (map toLower z) <> "_"
    remType :: String -> [IVar] -> [IVar]
    remType _ [] = []
    remType t ((t2,n):ts) = if t == t2
        then remType t ts
        else (t2,n) : remType t ts


{- **** Implementation (.C) File Functions **** -}

--Makes the .C file
mkCFile :: CF -> String
mkCFile cf = unlines
 [
  header,
  concatMap (prDataC user) (getAbstractSyntax cf),
  definedRules (Just $ LC nil cons) cf
  "/********************   Defined Constructors    ********************/"
 ]
 where
  nil _  = (,dummyType) $ "NULL"
  cons t = (,dummyType) $ "new List" ++ identType t
  user   = map fst (tokenPragmas cf)
  header = unlines
   [
    "//C++ Abstract Syntax Implementation generated by the BNF Converter.",
    "#include <algorithm>",
    "#include \"Absyn.H\""
   ]

--This is not represented in the implementation.
prDataC :: [UserDef] -> Data -> String
prDataC user (cat, rules) = concatMap (prRuleC user cat) rules

--Classes for rules vary based on the type of rule.
prRuleC :: [UserDef] -> Cat -> (String, [Cat]) -> String
prRuleC user c (fun, cats) =
    if isNilFun fun || isOneFun fun
    then ""  --these are not represented in the AbSyn
    else if isConsFun fun
    then --this is the linked list case.
    unlines
    [
     "/********************   " ++ c' ++ "    ********************/",
     render $ prConstructorC user c' vs cats,
     prCopyC user c' vs,
     prDestructorC user c' vs,
     prListFuncs user c',
     prAcceptC c',
     prCloneC user c' vs,
     ""
    ]
    else --a standard rule
    unlines
    [
     "/********************   " ++ fun ++ "    ********************/",
     render $ prConstructorC user fun vs cats,
     prCopyC user fun vs,
     prDestructorC user fun vs,
     prAcceptC fun,
     prCloneC user fun vs,
     ""
    ]
   where
     vs = getVars cats
     c' = identCat (normCat c)

--These are all built-in list functions.
--Later we could include things like lookup,insert,delete,etc.
prListFuncs :: [UserDef] -> String -> String
prListFuncs user c = unlines
 [
  c ++ "::" ++ c ++ "(" ++ m +++ mstar ++ "p)",
  "{",
  "  " ++ m' ++ " = p;",
  "  " ++ v ++ "= 0;",
  "}",
  c ++ "*" +++ c ++ "::" ++ "reverse()",
  "{",
  "  if (" ++ v +++ "== 0) return this;",
  "  else",
  "  {",
  "    " ++ c ++ " *tmp =" +++ v ++ "->reverse(this);",
  "    " ++ v +++ "= 0;",
  "    return tmp;",
  "  }",
  "}",
  "",
  c ++ "*" +++ c ++ "::" ++ "reverse(" ++ c ++ "* prev)",
  "{",
  "  if (" ++ v +++ "== 0)",
  "  {",
  "    " ++ v +++ "= prev;",
  "    return this;",
  "  }",
  "  else",
  "  {",
  "    " ++ c +++ "*tmp =" +++ v ++ "->reverse(this);",
  "    " ++ v +++ "= prev;",
  "    return tmp;",
  "  }",
  "}"
 ]
 where
   v = map toLower c ++ "_"
   m = drop 4 c
   mstar = if isBasic user m then "" else "*"
   m' = drop 4 v

--The standard accept function for the Visitor pattern
prAcceptC :: String -> String
prAcceptC ty =
  "\nvoid " ++ ty ++ "::accept(Visitor *v) { v->visit" ++ ty ++ "(this); }"

-- | The constructor just assigns the parameters to the corresponding instance
-- variables.
-- >>> prConstructorC ["Integer"] "bla" [("A",1), ("Integer",1), ("A",2)] [Cat "A", Cat "Integer", Cat "A"]
-- bla::bla(A *p1, Integer p2, A *p3) { a_1 = p1; integer_ = p2; a_2 = p3; }
prConstructorC :: [UserDef] -> String -> [IVar] -> [Cat] -> Doc
prConstructorC user c vs cats =
    text c <> "::" <> text c <> parens args
    <+> "{" <+> text (prAssigns vs params) <> "}"
  where
    (types, params) = unzip (prParams cats (length cats) (length cats+1))
    args = hsep $ punctuate "," $ zipWith prArg types params
    prArg type_ name
      | isBasic user type_  = text type_ <+> text name
      | otherwise           = text type_ <+> "*" <> text name

--Print copy constructor and copy assignment
prCopyC :: [UserDef] -> String -> [IVar] -> String
prCopyC user c vs =
    c ++ "::" ++ c ++ "(const" +++ c +++ "& other) {" +++
      concatMap doV vs ++++
      "}" ++++
      c +++ "&" ++ c ++ "::" ++ "operator=(const" +++ c +++ "& other) {" ++++
      "  " ++ c +++ "tmp(other);" ++++
      "  swap(tmp);" ++++
      "  return *this;" ++++
      "}" ++++
      "void" +++ c ++ "::swap(" ++ c +++ "& other) {" ++++
      concatMap swapV vs ++++
      "}\n"
    where  doV :: IVar -> String
           doV v@(t, _)
             | isBasic user t = "  " ++ vn v ++ " = other." ++ vn v ++ ";\n"
             | otherwise = "  " ++ vn v ++ " = other." ++ vn v ++ "->clone();\n"
           vn :: IVar -> String
           vn (t, 0) = varName t
           vn (t, n) = varName t ++ show n
           swapV :: IVar -> String
           swapV v = "  std::swap(" ++ vn v ++ ", other." ++ vn v ++ ");\n"

--The cloner makes a new deep copy of the object
prCloneC :: [UserDef] -> String -> [IVar] -> String
prCloneC _ c _ =
  c +++ "*" ++ c ++ "::clone() const {" ++++
    "  return new" +++ c ++ "(*this);\n}"

--The destructor deletes all a class's members.
prDestructorC :: [UserDef] -> String -> [IVar] -> String
prDestructorC user c vs  =
    c ++ "::~" ++ c ++"()" +++ "{" +++
    concatMap prDeletes vs ++ "}"
  where
    prDeletes :: (String, Int) -> String
    prDeletes (t, n)
        | isBasic user t = ""
        | n == 0 = "delete(" ++ varName t ++ "); "
        | otherwise = "delete(" ++ varName t ++ show n ++ "); "

--Prints the constructor's parameters.
prParams :: [Cat] -> Int -> Int -> [(String,String)]
prParams [] _ _ = []
prParams (c:cs) n m = (identCat c, "p" ++ show (m-n))
                    : prParams cs (n-1) m

--Prints the assignments of parameters to instance variables.
--This algorithm peeks ahead in the list so we don't use map or fold
prAssigns :: [IVar] -> [String] -> String
prAssigns [] _ = []
prAssigns _ [] = []
prAssigns ((t,n):vs) (p:ps) =
 if n == 1 then
  case findIndices (\(l,_) -> l == t) vs of
    [] -> varName t +++ "=" +++ p ++ ";" +++ prAssigns vs ps
    _ -> varName t ++ showNum n +++ "=" +++ p ++ ";" +++ prAssigns vs ps
 else varName t ++ showNum n +++ "=" +++ p ++ ";" +++ prAssigns vs ps


{- **** Helper Functions **** -}

-- | Checks if something is a basic or user-defined type.
isBasic :: [UserDef] -> String -> Bool
isBasic user x = x `elem` user || x `elem` specialCatsP
