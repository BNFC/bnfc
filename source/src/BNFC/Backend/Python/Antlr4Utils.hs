{-
  Description : Copied from the Java backend and modified for use with Python.
  Modified by : Björn Werner 
-}

module BNFC.Backend.Python.Antlr4Utils (getRuleName, getLabelName, startSymbol,
  comment) 
  where

import BNFC.CF
import BNFC.Utils (mkName, NameStyle(..))
import BNFC.Backend.Python.PyHelpers (pythonReserved)


-- | Make an Antlr grammar file line comment
comment :: String -> String
comment = ("// " ++)


-- Python keywords plus Antlr4 reserved keywords
pythonAntlrReserved :: [String]
pythonAntlrReserved = pythonReserved ++ 
  [ "catch"
  , "grammar"
  , "throws"
  ]


-- | Appends an underscore if there is a clash with a Python or ANTLR keyword.
--   E.g. "Grammar" clashes with ANTLR keyword "grammar" since
--   we sometimes need the upper and sometimes the lower case version
--   of "Grammar" in the generated parser.
getRuleName :: String -> String
getRuleName z
  -- | firstLowerCase z `elem` ("grammar" : pythonReserved) = z ++ "_"
  | z `elem` pythonAntlrReserved = z ++ "_"
  | otherwise = z


getLabelName :: Fun -> String
getLabelName = mkName ["Rule"] CamelCase


-- | Make a new entrypoint NT for an existing NT.
startSymbol :: String -> String
startSymbol = ("Start_" ++)
