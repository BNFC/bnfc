{-
    BNF Converter: Python PyParsing abstract syntax classes.
    Copyright (C) 2015  Author:  Gabriele Paganelli

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

    Description   : This module generates a file containing a 
                    collection of Python classes representing
                    the abstract syntax of the language being 
                    implemented.

    Author        : Gabriele Paganelli (gapag@distruzione.org)

    License       : GPL (GNU General Public License)

    Created       : 28 Jan, 2016

    Modified      :


   **************************************************************
-}

module BNFC.Backend.Python.CFtoPyAbsyn ( cf2PyAbsyn ) where

import BNFC.CF
import BNFC.Backend.Python.Utils
import BNFC.Backend.Python.AbsPython
import Text.PrettyPrint
 

-- should return a giant string
cf2PyAbsyn :: Bool -> String ->  CF -> String
cf2PyAbsyn addPosition _ cf = show $ vcat [
                            imports,
                            classes addPosition cf
                        ]

imports = text ""

classes :: Bool -> CF -> Doc
classes pos cf = absVcat $ 
                    concat 
                    [singlecategory pos cf (c,labs) | 
                        (c,labs) <- getUserCategories cf] 
-- todo change this signature. Take a look at CatDefs type in cftopyprinter.hs
-- third argument must be Cat -> [Rule]
-- Cat is parent class
-- [Rule] contains all children classes
singlecategory :: Bool -> CF -> (Cat, [(Fun, [Cat])]) -> [Entity]
singlecategory pos cf (category, labels) = 
        standard
        ++concat (map (childclass pos classname $ getUserTokens cf) labels)   
        where 
            
            classname = Ident $ identCat category 
            standard = 
                [Class classname NoInherit
                , IndentedBlock 
                    emptyConstructor
                ]
         

childclass :: Bool -> Ident -> [Cat] ->(Fun, [Cat]) -> [Entity]
childclass pos parent _ (name, fields) = 
        [
           Class (Ident name) (YesInherit parent)
           , IndentedBlock [Method $ signature, body] 
        ]
        where 
            --cleanlabels = map (\x -> if x `elem` usertokens then Cat "str" else x) fields
            cleanlabels = fields
            constr = constructor pos (name, cleanlabels)
            signature = fst constr
            body = snd constr


constructor :: Bool -> (Fun, [Cat]) -> (Entity, Entity)
constructor _ (name, fields) =
                        (   Function Init argList
                            , constructorBody name names
                        )
            where
                arginfo = getParams fields
                nameType= [Argument (Ident name) 
                                    NoType{-(YesType $ Ident typ)-}|(name, _)<- arginfo]
                argList = Self:nameType 
                names   = fst $ unzip arginfo  

constructorBody :: String -> [String] -> Entity
constructorBody name names = IndentedBlock $ [qname]++initialization
            where 
              qname = toNames [Function Super [Id (Ident name), Self ]
                              , Function Init []]
              instVars = map 
                          (\x -> instqname x ) 
                          names 
              instqname x = toNames [Self, Id (Ident x) ]
              fparnames = names
              initialization | null instVars = []
                             | otherwise     = assignments
              zipped = zip instVars fparnames
              assignments = [Assignment [x] [Id (Ident y) ] | (x,y) <- zipped]
