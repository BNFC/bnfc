{-
    BNF Converter: Datastructures for those languages who have multiple 
    backend tools.

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

    Description   : This module holds types and data structures
                    to be used for language backends that may
                    use multiple (lex|pars)er.
                    E.g. Java with jlex/jflex and cup vs Antlr4.
                             

    Author        : Gabriele Paganelli (gapag@distruzione.org)

    License       : GPL (GNU General Public License)

    Created       : 24 March, 2016

   **************************************************************
-}

module BNFC.Backend.Common.MultipleParserGenerationTools
 ( ToolParameters(..)
 , CFToParser(..)
 , CFToLexer(..)
 )
 where

import BNFC.CF
import BNFC.Backend.Common.Makefile
import BNFC.Backend.Common.NamedVariables (SymEnv)
import Text.PrettyPrint

type Action      = String
type MetaVar     = (String, Cat)

data ToolParameters = ToolParams{
    commentString     :: String,
    multilineComment  :: String -> String,
    preservePositions :: Bool,
    packageBase       :: String,
    packageAbsyn      :: String,
    generateAction    :: ToolParameters -> NonTerminal -> Fun -> [MetaVar] 
                            -> Bool -> Action,
    targetReservedWords    :: [String],
    lexerMembers     :: String ,
    parserMembers    :: String ,
    lexerHeader :: String ,
    parserHeader :: String 
}



-- | CF -> PARSER GENERATION TOOL BRIDGE
-- | function translating the CF to an appropriate parser generation tool.
type CF2ParserFunction = ToolParameters -> CF -> SymEnv -> String

-- | Chooses the translation from CF to the parser
data CFToParser = CF2Parse
    { cf2parse          :: CF2ParserFunction
    , makeparserdetails :: ToolParameters -> MakeFileDetails
    }
    
-- |CF -> LEXER GENERATION TOOL BRIDGE
-- | function translating the CF to an appropriate lexer generation tool.
type CF2LexerFunction = ToolParameters -> CF -> (Doc, SymEnv)

-- Chooses the translation from CF to the lexer
data CFToLexer = CF2Lex
    { cf2lex           :: CF2LexerFunction
    , makelexerdetails :: ToolParameters -> MakeFileDetails
    }
    