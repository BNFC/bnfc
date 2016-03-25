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

module BNFC.Backend.Python.CFtoPyPrinter ( cf2PyPrinter ) where

import BNFC.CF
import BNFC.Backend.Python.Utils
import BNFC.Backend.Python.AbsPython
cf2PyPrinter :: String -> CF -> String
cf2PyPrinter packageBase cf = "printer"


i :: Entity
i = mkId "i"

pp :: Entity
pp = mkId  "pp"

pprender :: Entity
pprender = mkId "render"

l_paren :: Entity
l_paren = mkId "_L_PAREN"

r_paren :: Entity
r_paren = mkId "_R_PAREN"

prettyPrintingEntryClass :: [Entity]
prettyPrintingEntryClass = 
    [Class (Ident  "PPEntry") NoInherit
    , methodDefinition Init [Self] body 
    ]
    where body = assigningConstructorBody ["sh_fun","pp_fun"]

pPContextClass :: [Entity]
pPContextClass = 
    [Class (Ident "PPContext") NoInherit
    , mdef Init consBody
    , mdef Enter enterBody
    , mdef Exit exitBody
    ]
    where 
        mdef         = \x y -> methodDefinition x [Self] y 
        consBody     = assigningConstructorBody ["pp", "i"]
        callRender x = Function (toNames [Self, pp, pprender]) [x]
        enterBody    = withBody l_paren
        exitBody     = withBody r_paren
        withBody x   = ifCascade [(condition, branch x)]
        condition    = toNames [Self,i]
        branch x     = [callRender $ toNames [Self, pp, x]]
                        
prettyPrinterClass :: CF -> [Entity]
prettyPrinterClass cf = 
    [ Class (Ident "PrettyPrinter") NoInherit] ++
    map IndentedBlock [ initDef cf
                            , renderDef
                            , pprintDef
                            , showDef
                            , __list_ppDef
                            , __list_shDef
                            , __pp_def cf
                            , __sh_def cf
                            , shDef 
                            , ppDef 
                            , printQuotedDef 
                            , indentDef 
                            , backupDef 
                            , trimDefDef 
                            ]
    where 
        absynCats = getUserCategories cf
        

initDef :: CF -> [Entity]
initDef cf = []

renderDef :: [Entity]
renderDef = []

pprintDef :: [Entity]
pprintDef = []
 
showDef :: [Entity]
showDef = []

__list_ppDef :: [Entity]
__list_ppDef = []

__list_shDef :: [Entity]
__list_shDef = []

__pp_def :: CF -> [Entity]
__pp_def cf = []

__sh_def :: CF -> [Entity]
__sh_def cf = []

shDef :: [Entity]
shDef = []
 
ppDef :: [Entity]
ppDef = []
 
printQuotedDef :: [Entity]
printQuotedDef = []
 
indentDef :: [Entity]
indentDef = []
 
backupDef :: [Entity]
backupDef = []
 
trimDefDef :: [Entity]
trimDefDef = [
    methodDefinition (mkId "trim") [] [
        toNames [Self, mkId "buf_"]    
        ]
    ]