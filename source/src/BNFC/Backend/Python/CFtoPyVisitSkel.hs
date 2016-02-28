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
cf2PyVisitSkel packageBase cf = show $ absVcat [
                                    imports packageBase
                                    visitorClass
                                ] 

{-show $ vcat [
                                            (text packageBase) <+> "Absyn"
                                            ,indent $ vcat[
                                                methods names
                                                ,dictionary names
                                                ,initmethod
                                                ,visit
                                            ]
                                            ]
                                where names = []
                     
-}

imports x = Import $ Ident (x++".Absyn")
{-
Idea is similar: this function returns a Doc and those below construct an object
using AbsPython objects, together with the information from the abstract syntax
Position is not necessary anymore
-}
visitorClass :: [Entity]
visitorClass = [Class (Ident "Visitor") NoInherit
                , IndentedBlock 
                    allVisitPrivate
                    ++ dictionary
                    ++ initMethod
                    ++ visitMethod
               ]

allVisitPrivate :: CF -> [Entity]
allVisitPrivate cf = concat 
                        [privateVisit cf (c,labs) | 
                          (c,labs) <- getAbstractSyntax cf, not $ isList c] 

privateVisit :: CF -> [Entity]
privateVisit cf = []

initMethod :: Entity
initMethod = emptyConstructor 

{-def visit(self, item, env):
        return self.index[item.__class__.__name__](self,item,env)
-}
visitMethod :: [Entity]
visitMethod = method (Left "visit") args body 
              where 
                body = [Return Qualified []]
                args = [self, item, env]
                self = Left Self
                item = ar "item"
                env = ar "env"
                ar s = Right (s, Nothing)


visitTypeMethod ::

methodDictionary ::
