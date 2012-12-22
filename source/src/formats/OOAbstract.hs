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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

module OOAbstract where

import CF
import Utils((+++),(++++))
import NamedVariables
import Data.List
import Data.Char(toLower)

-- A datastructure more appropriate than CF

data CAbs = CAbs {
  tokentypes :: [Cat],               -- user non-position token types
  listtypes  :: [(Cat,Bool)],        -- list types used, whether of classes
  absclasses :: [Cat],               -- grammar-def cats, normalized names
  conclasses :: [Fun],               -- constructors, except list ones
  signatures :: [(Cat,[CAbsRule])],  -- rules for each class, incl. pos tokens
  postokens  :: [Cat],               -- position token types
  defineds   :: [Fun]                -- defined (non-)constructors  
  }

-- (valcat,(constr,args)), True = is class (not basic), class variable stored
type CAbsRule = (Fun,[(Cat,Bool,String)]) 

-- all those names that denote classes in C++
allClasses :: CAbs -> [Cat]
allClasses ca = 
  absclasses ca ++ conclasses ca ++ map fst (listtypes ca) ++ postokens ca 

-- all those names that denote non-class types in C++
allNonClasses :: CAbs -> [Cat]
allNonClasses ca = 
  map fst basetypes ++ tokentypes ca

cf2cabs :: CF -> CAbs
cf2cabs cf = CAbs {
  tokentypes = toks,
  listtypes  = [(c, snd (status (drop 4 c))) | -- remove "List" from "ListC"
                  c <- map (normCat . identCat) lists],
  absclasses = nub $ map normCat cats,
  conclasses = [f | Just f <- map testRule (rulesOfCF cf)],
  signatures = posdata ++ map normSig (cf2data cf), 
  postokens  = pos,
  defineds   = defs
  }
 where
  (pos,base) = partition (isPositionCat cf) $ fst (unzip (tokenPragmas cf))
  (lists,cats) = partition isList $ allCats cf
  toks = map normCat base
  testRule (Rule f c r)
   | isList c  = Nothing
   | f == "_"  = Nothing
   | otherwise = Just f
  normSig (c,fcs) = 
    (identCat c,[(f, classVars (map (status . identCat) cs)) | (f,cs) <- fcs])
  posdata = 
    [("Visitable",  -- to give superclass
     [(c,[("String",False,"string_"),("Integer",False,"integer_")])]) | c<-pos]
  status cat = (cat, notElem cat (map fst basetypes ++ toks))
  defs = [f | FunDef f _ _ <- pragmasOfCF cf]

  classVars :: [(Cat,Bool)] -> [(Cat,Bool,String)]
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

isBaseType :: CAbs -> Cat -> Bool
isBaseType cf c = elem c $ tokentypes cf ++ map fst basetypes

classVar :: Cat -> String
classVar c = map toLower c ++ "_"

pointerIf :: Bool -> String -> String
pointerIf b v = if b then "*" ++ v else v
