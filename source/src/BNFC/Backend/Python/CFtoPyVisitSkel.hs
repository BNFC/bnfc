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

import BNFC.CF
cf2PyVisitSkel :: String -> CF -> String
cf2PyVisitSkel packageBase cf = "visitskel"
