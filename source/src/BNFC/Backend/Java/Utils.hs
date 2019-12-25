module BNFC.Backend.Java.Utils where

import BNFC.CF
import BNFC.Utils ( mkName, NameStyle(..))
import BNFC.Backend.Common.NamedVariables

javaReserved =
  [ "abstract"
  , "assert"
  , "boolean"
  , "break"
  , "byte"
  , "case"
  , "catch"
  , "char"
  , "class"
  , "const"
  , "continue"
  , "default"
  , "do"
  , "double"
  , "else"
  , "enum"
  , "extends"
  , "false"         -- there for Java/ANTLR backend
  , "final"
  , "finally"
  , "float"
  , "for"
  , "goto"
  , "if"
  , "implements"
  , "import"
  , "instanceof"
  , "int"
  , "interface"
  , "long"
  , "native"
  , "new"
  , "null"          -- there for Java/ANTLR backend
  -- , "Object"
  , "package"
  , "private"
  , "protected"
  , "public"
  , "return"
  , "short"
  , "static"
  , "strictfp"
  , "super"
  , "switch"
  , "synchronized"
  , "true"          -- there for Java/ANTLR backend
  , "this"
  , "throw"
  , "throws"
  , "transient"
  , "try"
  , "void"
  , "volatile"
  , "while"
  ]

-- | Append an underscore if there is a clash with a java or ANTLR keyword.
--   E.g. "Grammar" clashes with ANTLR keyword "grammar" since
--   we sometimes need the upper and sometimes the lower case version
--   of "Grammar" in the generated parser.
getRuleName :: String -> String
getRuleName z
  | firstLowerCase z `elem` ("grammar" : javaReserved) = z ++ "_"
  | otherwise = z

getLabelName :: Fun -> String
getLabelName = mkName ["Rule"] CamelCase

getLastInPackage :: String -> String
getLastInPackage =
    last . words . map (\c -> if c == '.' then ' ' else c)

-- | Make a new entrypoint NT for an existing NT.

startSymbol :: String -> String
startSymbol = ("Start_" ++)
