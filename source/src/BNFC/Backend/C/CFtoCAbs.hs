{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: C Abstract syntax
    Copyright (C) 2004  Author:  Michael Pellauer

    Description   : This module generates the C Abstract Syntax
                    tree classes. It generates both a Header file
                    and an Implementation file, and Appel's C
                    method.

    Author        : Michael Pellauer
    Created       : 15 September, 2003
-}

module BNFC.Backend.C.CFtoCAbs (cf2CAbs) where

import Prelude hiding ((<>))

import Control.Monad.State (State, gets, modify, evalState)

import Data.Char     ( toLower )
import Data.Either   ( lefts )
import Data.Function ( on )
import Data.List     ( groupBy, intercalate, intersperse, nub, sort )
import Data.Maybe    ( mapMaybe )
import Data.Set      ( Set )

import qualified Data.Set as Set

import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options  ( RecordPositions(..) )
import BNFC.Utils    ( (+++), unless )
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.C.Common ( posixC )


-- | The result is two files (.H file, .C file)
cf2CAbs
  :: RecordPositions
  -> String -- ^ Ignored.
  -> CF     -- ^ Grammar.
  -> (String, String) -- ^ @.H@ file, @.C@ file.
cf2CAbs rp _ cf = (mkHFile rp classes datas cf, mkCFile datas cf)
  where
  datas :: [Data]
  datas = getAbstractSyntax cf
  classes :: [String]
  classes = nub $ map (identCat . fst) datas

{- **** Header (.H) File Functions **** -}

-- | Makes the Header file.

mkHFile :: RecordPositions -> [String] -> [Data] -> CF -> String
mkHFile rp classes datas cf = unlines $ concat
  [ [ "#ifndef ABSYN_HEADER"
    , "#define ABSYN_HEADER"
    , ""
    ]
  , posixC
  , [ ""
    , "#include <stddef.h>  /* NULL */"
    , "#include <string.h>  /* strdup */"
    , ""
    , "/* C++ Abstract Syntax Interface.*/"
    , ""
    , prTypeDefs user
    , "/********************   Forward Declarations    ***********************/"
    ]
  , map prForward classes

  , [ "/********************   Abstract Syntax Classes    ********************/"
    , ""
    ]
  , map (prDataH rp) datas

  -- Cloning
  , unless (null classes) $ concat
    [ cloneComment
    , map prCloneH classes
    , [ "" ]
    ]

  -- Freeing
  , unless (null classes) $ concat
    [ destructorComment
    , map prFreeH classes
    , [ "" ]
    ]

  , unless (null definedConstructors)
    [ "/********************   Defined Constructors    ***********************/"
    , ""
    ]
  , intersperse "" $ map (prDefH user) definedConstructors

  , [ ""
    , "#endif"
    ]
  ]
  where
  user  :: [TokenCat]
  user   = tokenNames cf
  prForward :: String -> String
  prForward s = unlines
    [ "struct " ++ s ++ "_;"
    , "typedef struct " ++ s ++ "_ *" ++ s ++ ";"
    ]
  prCloneH :: String -> String
  prCloneH s = s ++ " clone_" ++ s ++ "(" ++ s ++ " p);"
  prFreeH :: String -> String
  prFreeH s = "void free_" ++ s ++ "(" ++ s ++ " p);"
  definedConstructors = definitions cf

cloneComment :: [String]
cloneComment =
  [ "/***************************   Cloning   ******************************/"
  , ""
  ]

destructorComment :: [String]
destructorComment =
  [ "/********************   Recursive Destructors    **********************/"
  , ""
  , "/* These free an entire abstract syntax tree"
  , " * including all subtrees and strings."
  , " *"
  , " * Will not work properly if there is sharing in the tree,"
  , " * i.e., when some pointers are aliased.  In this case"
  , " * it will attempt to free the same memory twice."
  , " */"
  , ""
  ]

-- | For @define@d constructors, make a CPP definition.
--
-- >>> prDefH [] (Define "iSg" [("i",undefined)] (App "ICons" undefined [Var "i", App "INil" undefined []]) undefined)
-- "#define make_iSg(i) \\\n  make_ICons (i, make_INil())"
--
-- >>> prDefH [] (Define "snoc" (map (,undefined) ["xs","x"]) (App "Cons" undefined [Var "x", Var "xs"]) undefined)
-- "#define make_snoc(xs,x) \\\n  make_Cons (x, xs)"
--
prDefH
  :: [TokenCat] -- ^ Names of the token constructors (silent in C backend).
  -> Define
  -> String
prDefH tokenCats (Define fun args e _t) =
  concat [ "#define make_", f, "(", intercalate "," xs, ") \\\n  ", prExp e `evalState` mempty ]
  where
  f  = funName fun
  xs = map fst args

  toCat :: Base -> Cat
  toCat = catOfType $ specialCatsP ++ tokenCats

  -- Issue #363, #348.
  -- Duplicate occurrences of variables in expression need to be cloned,
  -- because deallocation assumes that the AST is in fact a tree.
  -- Duplicate occurrences introduce sharing and thus turn it into a DAG
  -- (directed acyclic graph).
  -- We maintain a set of variables we have already encountered.
  prExp :: Exp -> State (Set String) String
  prExp = \case

    Var x -> gets (Set.member x) >>= \case
      -- The first use is not cloned.
      False -> x <$ modify (Set.insert x)
      -- Subsequent uses are cloned.
      True  -> case lookup x args of
        Just t -> return $ cloner (toCat t) x
        Nothing -> undefined -- impossible

    -- Andreas, 2021-02-13, issue #338
    -- Token categories are just @typedef@s in C, so no constructor needed.
    App g _ [e] | g `elem` tokenCats
                -> prExp e
    App "[]" _ [] -> return "NULL"
    App g t es  -> do
      es' <- mapM prExp es
      return $ concat [ "make_", con g t, lparen es, intercalate ", " es', ")" ]
    LitInt    i -> return $ show i
    LitDouble d -> return $ show d
    LitChar   c -> return $ show c
    LitString s -> return $ concat [ "strdup(", show s, ")" ]  -- so that free() does not crash!
  con g ~(FunT _ts t)
    | isConsFun g = identType t
    | otherwise   = g
  -- If more than one argument, or complex argument, put space before opening parenthesis.
  lparen = \case
    _:_:_           -> " ("
    [App _ _ (_:_)] -> " ("
    _               -> "("

-- | Prints struct definitions for all categories.
prDataH :: RecordPositions -> Data -> String
prDataH rp (cat, rules)
  | isList cat = unlines
      [ "struct " ++ c' ++ "_"
      , "{"
      , "  " ++ mem +++ varName mem ++ ";"
      , "  " ++ c' +++ varName c' ++ ";"
      , "};"
      , ""
      , c' ++ " make_" ++ c' ++ "(" ++ mem ++ " p1, " ++ c' ++ " p2);"
      ]
  | otherwise = unlines $ concat
    [ [ "struct " ++ identCat cat ++ "_"
      , "{"
      ]
    , [ "  int line_number, char_number;" | rp == RecordPositions ]
    , [ "  enum { " ++ intercalate ", " (map prKind rules) ++ " } kind;"
      , "  union"
      , "  {"
      , concatMap prUnion rules ++ "  } u;"
      , "};"
      , ""
      ]
    , concatMap (prRuleH cat) rules
    ]
  where
    c' = identCat (normCat cat)
    mem = identCat (normCatOfList cat)
    prKind (fun, _) = "is_" ++ fun
    prUnion (_, []) = ""
    prUnion (fun, cats) = "    struct { " ++ (render $ prInstVars (getVars cats)) ++ " } " ++ (memName fun) ++ ";\n"


-- | Interface definitions for rules vary on the type of rule.
prRuleH :: Cat -> (Fun, [Cat]) -> [String]
prRuleH c (fun, cats)
  | isNilFun fun || isOneFun fun || isConsFun fun = [] -- these are not represented in the AbSyn
  | otherwise = return $ concat
      [ catToStr c, " make_", fun, "(", prParamsH (getVars cats), ");" ]
  where
    prParamsH :: [(String, a)] -> String
    prParamsH [] = "void"
    prParamsH ps = intercalate ", " $ zipWith par ps [0::Int ..]
      where par (t, _) n = t ++ " p" ++ show n

-- typedefs in the Header make generation much nicer.
prTypeDefs :: [String] -> String
prTypeDefs user = unlines $ concat
  [ [ "/********************   TypeDef Section    ********************/"
    , ""
    , "typedef int Integer;"
    , "typedef char Char;"
    , "typedef double Double;"
    , "typedef char* String;"
    , "typedef char* Ident;"
    ]
  , map prUserDef user
  ]
  where
    prUserDef s = "typedef char* " ++ s ++ ";"

-- | A class's instance variables. Print the variables declaration by grouping
-- together the variables of the same type.
-- >>> prInstVars [("A", 1)]
-- A a_1;
-- >>> prInstVars [("A",1),("A",2),("B",1)]
-- A a_1, a_2; B b_1;
prInstVars :: [IVar] -> Doc
prInstVars =
    hsep . map prInstVarsOneType . groupBy ((==) `on` fst) . sort
  where
    prInstVarsOneType ivars = text (fst (head ivars))
                              <+> hsep (punctuate comma (map prIVar ivars))
                              <> semi
    prIVar (s, i) = text (varName s) <> text (showNum i)

{- **** Implementation (.C) File Functions **** -}

-- | Makes the .C file
mkCFile :: [Data] -> CF -> String
mkCFile datas _cf = concat
  [ header
  , render $ vsep $ concatMap prDataC datas
  , unlines [ "", "" ]
  -- Cloning
  , unlines cloneComment
  , unlines $ concatMap prCloneC datas
  -- Freeing
  , unlines destructorComment
  , unlines $ concatMap prDestructorC datas
  ]
  where
  header = unlines
    [ "/* C Abstract Syntax Implementation. */"
    , ""
    , "#include <stdio.h>"
    , "#include <stdlib.h>"
    , "#include \"Absyn.h\""
    , ""
    ]

-- |
-- >>> text $ unlines $ prCloneC (Cat "Exp", [("EInt", [TokenCat "Integer"]), ("EAdd", [Cat "Exp", Cat "Exp"])])
-- Exp clone_Exp(Exp p)
-- {
--   switch(p->kind)
--   {
--   case is_EInt:
--     return make_EInt (p->u.eint_.integer_);
-- <BLANKLINE>
--   case is_EAdd:
--     return make_EAdd
--       ( clone_Exp(p->u.eadd_.exp_1)
--       , clone_Exp(p->u.eadd_.exp_2)
--       );
-- <BLANKLINE>
--   default:
--     fprintf(stderr, "Error: bad kind field when cloning Exp!\n");
--     exit(1);
--   }
-- }
-- <BLANKLINE>
-- <BLANKLINE>
prCloneC :: Data -> [String]
prCloneC (cat, rules)
  | isList cat =
    [ cl ++ " clone_" ++ cl ++ "("++ cl +++ vname ++ ")"
    , "{"
    , "  if (" ++ vname ++ ")"
    , "  {"
    , "    /* clone of non-empty list */"
    , render $ prettyList 6 (text $ "    return make_" ++ cl) "(" ");" ","
        [ text $ visitMember
        , text $ "clone_" ++ cl ++ "(" ++ vname ++ "->" ++ vname ++ "_)"
        ]
    , "  }"
    , "  else return NULL; /* clone of empty list */"
    , "}"
    , ""
    ]
  | otherwise = concat
    [ [ cl ++ " clone_" ++ cl ++ "(" ++ cl ++ " p)"
      , "{"
      , "  switch(p->kind)"
      , "  {"
      ]
    , concatMap prCloneRule rules
    , [ "  default:"
      , "    fprintf(stderr, \"Error: bad kind field when cloning " ++ cl ++ "!\\n\");"
      , "    exit(1);"
      , "  }"
      , "}"
      , ""
      ]
    ]
  where
  cl          = identCat cat
  vname       = map toLower cl
  visitMember :: String
  visitMember = cloner el $ vname ++ "->" ++ member ++ "_"
    where
    el     = normCatOfList cat
    member = map toLower $ identCat el

  prCloneRule :: (String, [Cat]) -> [String]
  prCloneRule (fun, cats) | not (isCoercion fun) =
    [ "  case is_" ++ fnm ++ ":"
    , render $ prettyList 6 (text $ "    return make_" ++ fnm) "(" ");\n" "," $
        map (text . prCloneCat fnm) $ lefts $ numVars $ map Left cats
    ]
    where
    fnm = funName fun
  prCloneRule _ = []

  -- | This goes on to recurse to the instance variables.

  prCloneCat :: String -> (Cat, Doc) -> String
  prCloneCat fnm (cat, nt) = cloner cat member
    where
    member = concat [ "p->u.", map toLower fnm, "_.", render nt ]

-- | Clone or not depending on the category.
--   Only pointers need to be cloned.
--
cloner :: Cat -> String -> String
cloner cat x =
  case cat of
    TokenCat c
      | c `elem` ["Char", "Double", "Integer"]
                  -> x
      | otherwise -> "strdup" ++ parens x
    _             -> "clone_" ++ identCat (normCat cat) ++ parens x
  where parens = ("(" ++) . (++ ")")


-- |
-- >>> text $ unlines $ prDestructorC (Cat "Exp", [("EInt", [TokenCat "Integer"]), ("EAdd", [Cat "Exp", Cat "Exp"])])
-- void free_Exp(Exp p)
-- {
--   switch(p->kind)
--   {
--   case is_EInt:
--     break;
-- <BLANKLINE>
--   case is_EAdd:
--     free_Exp(p->u.eadd_.exp_1);
--     free_Exp(p->u.eadd_.exp_2);
--     break;
-- <BLANKLINE>
--   default:
--     fprintf(stderr, "Error: bad kind field when freeing Exp!\n");
--     exit(1);
--   }
--   free(p);
-- }
-- <BLANKLINE>
-- <BLANKLINE>
prDestructorC :: Data -> [String]
prDestructorC (cat, rules)
  | isList cat = concat
    [ [ "void free_" ++ cl ++ "("++ cl +++ vname ++ ")"
      , "{"
      , "  if (" ++ vname ++ ")"
      , "  {"
      ]
    , map ("    " ++) visitMember
    , [ "    free_" ++ cl ++ "(" ++ vname ++ "->" ++ vname ++ "_);"
      , "    free(" ++ vname ++ ");"
      , "  }"
      , "}"
      , ""
      ]
    ]
  | otherwise = concat
    [ [ "void free_" ++ cl ++ "(" ++ cl ++ " p)"
      , "{"
      , "  switch(p->kind)"
      , "  {"
      ]
    , concatMap prFreeRule rules
    , [ "  default:"
      , "    fprintf(stderr, \"Error: bad kind field when freeing " ++ cl ++ "!\\n\");"
      , "    exit(1);"
      , "  }"
      , "  free(p);"
      , "}"
      , ""
      ]
    ]
  where
  cl          = identCat cat
  vname       = map toLower cl
  visitMember =
    case ecat of
      TokenCat c
        | c `elem` ["Char", "Double", "Integer"] -> []
        | otherwise -> [ "free" ++ rest ]
      _             -> [ "free_" ++ ecl ++ rest ]
    where
    rest   = "(" ++ vname ++ "->" ++ member ++ "_);"
    member = map toLower ecl
    ecl    = identCat ecat
    ecat   = normCatOfList cat

  prFreeRule :: (String, [Cat]) -> [String]
  prFreeRule (fun, cats) | not (isCoercion fun) = concat
    [ [ "  case is_" ++ fnm ++ ":"
      ]
    , map ("    " ++) $ mapMaybe (prFreeCat fnm) $ lefts $ numVars $ map Left cats
    , [ "    break;"
      , ""
      ]
    ]
    where
    fnm = funName fun
  prFreeRule _ = []

  -- | This goes on to recurse to the instance variables.

  prFreeCat :: String -> (Cat, Doc) -> Maybe String
  prFreeCat _fnm (TokenCat c, _nt)
    | c `elem` ["Char", "Double", "Integer"] = Nothing
      -- Only pointer need to be freed.
  prFreeCat fnm (cat, nt) = Just $ concat
      [ maybe ("free_" ++ identCat (normCat cat)) (const "free") $ maybeTokenCat cat
      , "(p->u."
      , map toLower fnm
      , "_.", render nt, ");"
      ]



prDataC :: Data -> [Doc]
prDataC (cat, rules) = map (prRuleC cat) rules

-- | Classes for rules vary based on the type of rule.
--
-- * Empty list constructor, these are not represented in the AbSyn
--
-- >>> prRuleC (ListCat (Cat "A")) ("[]", [Cat "A", Cat "B", Cat "B"])
-- <BLANKLINE>
--
-- * Linked list case. These are all built-in list functions.
-- Later we could include things like lookup, insert, delete, etc.
--
-- >>> prRuleC (ListCat (Cat "A")) ("(:)", [Cat "A", Cat "B", Cat "B"])
-- /********************   ListA    ********************/
-- <BLANKLINE>
-- ListA make_ListA(A p1, ListA p2)
-- {
--     ListA tmp = (ListA) malloc(sizeof(*tmp));
--     if (!tmp)
--     {
--         fprintf(stderr, "Error: out of memory when allocating ListA!\n");
--         exit(1);
--     }
--     tmp->a_ = p1;
--     tmp->lista_ = p2;
--     return tmp;
-- }
--
-- * Standard rule
--
-- >>> prRuleC (Cat "A") ("funa", [Cat "A", Cat "B", Cat "B"])
-- /********************   funa    ********************/
-- <BLANKLINE>
-- A make_funa(A p1, B p2, B p3)
-- {
--     A tmp = (A) malloc(sizeof(*tmp));
--     if (!tmp)
--     {
--         fprintf(stderr, "Error: out of memory when allocating funa!\n");
--         exit(1);
--     }
--     tmp->kind = is_funa;
--     tmp->u.funa_.a_ = p1;
--     tmp->u.funa_.b_1 = p2;
--     tmp->u.funa_.b_2 = p3;
--     return tmp;
-- }
prRuleC :: Cat -> (String, [Cat]) -> Doc
prRuleC _ (fun, _) | isNilFun fun || isOneFun fun = empty
prRuleC cat@(ListCat c') (fun, _) | isConsFun fun = vcat'
    [ "/********************   " <> c <> "    ********************/"
    , ""
    , c <+> "make_" <> c <> parens (text m <+> "p1" <> "," <+> c <+> "p2")
    , lbrace
    , nest 4 $ vcat'
        [ c <+> "tmp = (" <> c <> ") malloc(sizeof(*tmp));"
        , "if (!tmp)"
        , lbrace
        , nest 4 $ vcat'
            [ "fprintf(stderr, \"Error: out of memory when allocating " <> c <> "!\\n\");"
            , "exit(1);" ]
        , rbrace
        , text $ "tmp->" ++ m' ++ " = " ++ "p1;"
        , "tmp->" <> v <+> "=" <+> "p2;"
        , "return tmp;" ]
    , rbrace ]
  where
    icat = identCat (normCat cat)
    c = text icat
    v = text (map toLower icat ++ "_")
    m = identCat (normCat c')
    m' = map toLower m ++ "_"
prRuleC c (fun, cats) = vcat'
    [ text $ "/********************   " ++ fun ++ "    ********************/"
    , ""
    , prConstructorC c fun vs cats ]
  where
    vs = getVars cats

-- | The constructor just assigns the parameters to the corresponding instance
-- variables.
-- >>> prConstructorC (Cat "A") "funa" [("A",1),("B",2)] [Cat "O", Cat "E"]
-- A make_funa(O p1, E p2)
-- {
--     A tmp = (A) malloc(sizeof(*tmp));
--     if (!tmp)
--     {
--         fprintf(stderr, "Error: out of memory when allocating funa!\n");
--         exit(1);
--     }
--     tmp->kind = is_funa;
--     tmp->u.funa_.a_ = p1;
--     tmp->u.funa_.b_2 = p2;
--     return tmp;
-- }
prConstructorC :: Cat -> String -> [IVar] -> [Cat] -> Doc
prConstructorC cat c vs cats = vcat'
    [ text (cat' ++ " make_" ++ c) <> parens args
    , lbrace
    , nest 4 $ vcat'
        [ text $ cat' ++ " tmp = (" ++ cat' ++ ") malloc(sizeof(*tmp));"
        , text "if (!tmp)"
        , lbrace
        , nest 4 $ vcat'
            [ text ("fprintf(stderr, \"Error: out of memory when allocating " ++ c ++ "!\\n\");")
            , text "exit(1);" ]
        , rbrace
        , text $ "tmp->kind = is_" ++ c ++ ";"
        , prAssigns c vs params
        , text "return tmp;" ]
    , rbrace ]
  where
    cat' = identCat (normCat cat)
    (types, params) = unzip (prParams cats)
    args = hsep $ punctuate comma $ zipWith (<+>) types params

-- | Prints the constructor's parameters. Returns pairs of type * name
-- >>> prParams [Cat "O", Cat "E"]
-- [(O,p1),(E,p2)]
prParams :: [Cat] -> [(Doc, Doc)]
prParams = zipWith prParam [1::Int ..]
  where
    prParam n c = (text (identCat c), text ("p" ++ show n))

-- | Prints the assignments of parameters to instance variables.
-- >>> prAssigns "A" [("A",1),("B",2)] [text "abc", text "def"]
-- tmp->u.a_.a_ = abc;
-- tmp->u.a_.b_2 = def;
prAssigns :: String -> [IVar] -> [Doc] -> Doc
prAssigns c vars params = vcat $ zipWith prAssign vars params
  where
    prAssign (t,n) p =
        text ("tmp->u." ++ c' ++ "_." ++ vname t n) <+> char '=' <+> p <> semi
    vname t n
      | n == 1, [_] <- filter ((t ==) . fst) vars
                  = varName t
      | otherwise = varName t ++ showNum n
    c' = map toLower c

{- **** Helper Functions **** -}

memName :: String -> String
memName s = map toLower s ++ "_"
