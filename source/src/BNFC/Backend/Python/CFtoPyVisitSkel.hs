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

module BNFC.Backend.Python.CFtoPyVisitSkel ( cf2PyVisitSkel ) where
import BNFC.Backend.Python.Utils
import BNFC.Backend.Python.AbsPython
import BNFC.CF
import Text.PrettyPrint
cf2PyVisitSkel :: String -> CF -> String
cf2PyVisitSkel packageBase cf = show $ absVcat $ [imports packageBase] ++(visitorClass cf)


imports x = From $ Ident "Absyn"
{-
Idea is similar: this function returns a Doc and those below construct an object
using AbsPython objects, together with the information from the abstract syntax
Position is not necessary anymore
-}


visitorClass :: CF -> [Entity]
visitorClass cf = [Class (Ident "Visitor") NoInherit
                , IndentedBlock $ 
                    (allVisitPrivate cf absynCats)
                    ++ [Dictionary (dictionary typeContribution absynCats)]
                    ++ initMethod
                    ++ visitMethod
               ]
               where 
                 absynCats = getUserCategories cf

allVisitPrivate :: CF -> [(Cat, [(Fun, [Cat])])] -> [Entity]
allVisitPrivate cf [] = []
allVisitPrivate cf ((c,labels):rest) = (privateVisit cf labels)
                                    ++allVisitPrivate cf rest  

privateVisitorEntity :: Entity -> String
privateVisitorEntity (Id (Ident x)) = privateVisitorName x
privateVisitorEntity _ = "WRONG TYPE!"
 
privateVisitorName :: Fun -> String
privateVisitorName x = "__visit_"++x
 

privateVisit :: CF -> [(Fun, [Cat])] -> [Entity]
privateVisit cf [] = []
privateVisit cf ((typ,content):rest) = method pvisit 
                                        [Left Self, ar itemStr, ar envStr]
                                        (visitBody $ zip filt params)++ (privateVisit cf rest) 
                                    where
                                      filt = (filterTerminals cf content)
                                      params = getParams filt
                                      pvisit = Left $ privateVisitorName typ 

-- this creates the visit method's body.
-- for each list object, creates a for loop;
-- for each non-token object calls the dictionary.
visitBody :: [(Cat,(String,String))] -> [Entity]
visitBody [] = [Pass]
visitBody ((c,(name,typ)):cs) = action -- ++ visitBody cs
                    where 
                     id = mkId name 
                     action = if isList c
                              then mkFor id
                              else [mkVisit $ toNames[mkId itemStr, id] ]

mkFor :: Entity -> [Entity]
mkFor e = [
                        For loopVar (toNames [mkId itemStr, e]),
                        IndentedBlock [
                            call
                        ]
                    ] 
                    where
                      loopVar = mkId "x"
                      call = visitCall loopVar
                    

mkVisit :: Entity -> Entity
mkVisit e = visitCall $ e
                      
visitCall :: Entity -> Entity
visitCall name = Function vcall [name, ienv]
                   where
                     vcall = toNames [Self, mkId "visit"]
                     ienv = mkId envStr
                      
                      
initMethod :: [Entity]
initMethod = emptyConstructor 


typeContribution :: Entity -> Entity
typeContribution f =  Entry f (mkId $ privateVisitorEntity f)

-- fun is a 
classEntries :: (Fun, [Cat]) -> [Entity]
classEntries (c, labs)= []



envStr = "env"
itemStr = "item"
ar s = Right (s, Nothing)



{-def visit(self, item, env):
        return self.index[item.__class__.__name__](self,item,env)
-}
visitMethod :: [Entity]
visitMethod = method (Left "visit") args body 
              where 
                body = [Return $ Function query [Self, iitem, ienv]]
                query = toNames [Self, dictionaryLookup iitem]
                args = [self, item, env]
                self = Left Self
                item = ar itemStr
                env = ar envStr
                iitem = mkId itemStr
                ienv = mkId envStr


{-
visitTypeMethod ::

methodDictionary ::
-}