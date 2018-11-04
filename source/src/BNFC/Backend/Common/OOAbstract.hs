{-
    BNF Converter: Datastructure for object-oriented abstract syntax generators
    Copyright (C) 2006  Author:  Aarne Ranta

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

    Description   : This module defines a data structure that is
                    used for generating abstract syntax in cpp_stl.
                    It should be used in other STL modules as well,
                    and could be used for object-oriented languages
                    in general, to avoid duplicated work.

    Author        : Aarne Ranta (aarne@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 29 August, 2006

    Modified      : 29 August, 2006 / Aarne Ranta


   **************************************************************
-}

module BNFC.Backend.Common.OOAbstract where

import BNFC.CF
import Data.List
import Data.Char(toLower)

-- A datastructure more appropriate than CF

data CAbs = CAbs {
  tokentypes :: [String],               -- user non-position token types
  listtypes  :: [(String,Bool)],        -- list types used, whether of classes
  absclasses :: [String],               -- grammar-def cats, normalized names
  conclasses :: [Fun],               -- constructors, except list ones
  signatures :: [(String,[CAbsRule])],  -- rules for each class, incl. pos tokens
  postokens  :: [String],               -- position token types
  defineds   :: [Fun]                -- defined (non-)constructors
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
cf2cabs cf = CAbs {
  tokentypes = toks,
  listtypes  = [(c, snd (status (drop 4 c))) | -- remove "List" from "ListC"
                  c <- map (identCat . normCat) lists],
  absclasses = nub $ map (show.normCat) cats,
  conclasses = [f | Just f <- map testRule (cfgRules cf)],
  signatures = posdata ++ map normSig (cf2data cf),
  postokens  = map show pos,
  defineds   = defs
  }
 where
  (pos,base) = partition (isPositionCat cf) $ fst (unzip (tokenPragmas cf))
  (lists,cats) = partition isList $ allCatsNorm cf
  toks = map (show.normCat) base
  testRule (Rule f c _)
   | isList c  = Nothing
   | f == "_"  = Nothing
   | otherwise = Just f
  normSig (c,fcs) =
    (identCat c,[(f, classVars (map (status . identCat) cs)) | (f,cs) <- fcs])
  posdata =
    [("Visitable",  -- to give superclass
     [(show c,[("String",False,"string_"),("Integer",False,"integer_")])]) | c<-pos]
  status cat = (cat, notElem cat (map fst basetypes ++ toks))
  defs = [f | FunDef f _ _ <- cfgPragmas cf]

  classVars :: [(String,Bool)] -> [(String,Bool,String)]
  classVars cs =
    [(c,b,s) | ((c,b),s) <- zip cs (vars [] (map (classVar . fst) cs))]
  --- creating new names is quadratic, but parameter lists are short
  --- this should conform with Michael's naming
  vars seen vv = case vv of
    []   -> vv
    v:vs -> case length (filter (==v) seen) of
      0 | elem v vs -> (v ++ "1"): vars (v:seen) vs
      0             -> v         : vars (v:seen) vs
      n             -> (v ++ show (n+1)) : vars (v:seen) vs

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
