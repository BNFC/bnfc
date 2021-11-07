{-# LANGUAGE LambdaCase #-}

{-
    BNF Converter: Datastructure for object-oriented abstract syntax generators
    Copyright (C) 2006  Author:  Aarne Ranta

    Description   : This module defines a data structure that is
                    used for generating abstract syntax in cpp_stl.
                    It should be used in other STL modules as well,
                    and could be used for object-oriented languages
                    in general, to avoid duplicated work.

    Author        : Aarne Ranta (aarne@cs.chalmers.se)
    Created       : 29 August, 2006

-}

module BNFC.Backend.Common.OOAbstract where

import Data.Char  (toLower)
import qualified Data.List as List
import Data.Maybe

import BNFC.CF

-- A datastructure more appropriate than CF

data CAbs = CAbs {
  tokentypes :: [String],               -- user non-position token types
  listtypes  :: [(String,Bool)],        -- list types used, whether of classes
  absclasses :: [String],               -- grammar-def cats, normalized names
  conclasses :: [Fun],                  -- constructors, except list ones
  signatures :: [(String,[CAbsRule])],  -- rules for each class, incl. pos tokens
  postokens  :: [String],               -- position token types
  defineds   :: [Fun]                   -- defined (non-)constructors
  }

-- (valcat,(constr,args)), True = is class (not basic), class variable stored
type CAbsRule = (Fun,[(String,Bool,String)])

-- all those names that denote classes in C++
allClasses :: CAbs -> [String]
allClasses ca =
  absclasses ca ++ conclasses ca ++ map fst (listtypes ca) ++ postokens ca

-- all those names that denote non-class types in C++
allNonClasses :: CAbs -> [String]
allNonClasses ca =
  map fst basetypes ++ tokentypes ca

cf2cabs :: CF -> CAbs
cf2cabs cf = CAbs
  { tokentypes = toks
  , listtypes  = [(c, snd (status (drop 4 c))) | -- remove "List" from "ListC"
                  c <- map (identCat . normCat) lists]
  , absclasses = List.nub $ map (identCat . normCat) cats -- NB: does not include list categories
  , conclasses = mapMaybe testRule $ cfgRules cf
  , signatures = posdata ++ map normSig (cf2data cf)
  , postokens  = pos
  , defineds   = defs
  }
 where
  (pos,  toks) = List.partition (isPositionCat cf) $ map fst $ tokenPragmas cf
  (lists,cats) = List.partition isList $ allCatsNorm cf
  testRule (Rule f c _ _)
   | isList (wpThing c)  = Nothing
   | funName f == "_"  = Nothing
   | otherwise = Just $ funName f
  normSig (c,fcs) =
    (identCat c,[(f, classVars (map (status . identCat) cs)) | (f,cs) <- fcs])
  posdata =
    [("Visitable",  -- to give superclass
     [(c,[("String",False,"string_"),("Integer",False,"integer_")])]) | c<-pos]
  status cat = (cat, notElem cat (map fst basetypes ++ toks))
  defs = [ funName f | FunDef (Define f _ _ _) <- cfgPragmas cf]

  classVars :: [(String,Bool)] -> [(String,Bool,String)]
  classVars cs =
    [(c,b,s) | ((c,b),s) <- zip cs (vars [] (map (classVar . fst) cs))]
  --- creating new names is quadratic, but parameter lists are short
  --- this should conform with Michael's naming
  vars seen = \case
    []   -> []
    v:vs -> case length (filter (==v) seen) of
      0 | elem v vs -> (v ++ "1"): vars (v:seen) vs
      0             -> v         : vars (v:seen) vs
      n             -> (v ++ show (n+1)) : vars (v:seen) vs

basetypes :: [ (String, String) ]
basetypes = [
  ("Integer","int"),
  ("Char",   "char"),
  ("Double", "double"),
  ("String", "std::string"),
  ("Ident",  "std::string")
  ]

classVar :: String -> String
classVar c = map toLower c ++ "_"

pointerIf :: Bool -> String -> String
pointerIf b v = if b then "*" ++ v else v

wrapUniquePtrIf :: Bool -> String -> String
wrapUniquePtrIf b v = if b then "std::unique_ptr<" ++v++">" else v

wrapMoveIf :: Bool -> String -> String
wrapMoveIf b v = if b then "std::move(" ++v++")" else v
