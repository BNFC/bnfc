module BNFC.Backend.Antlr.Utils where

import BNFC.CF (Fun)
import BNFC.Utils ( mkName, NameStyle(..))

getRuleName :: String -> String
getRuleName z = if z == "grammar" then z ++ "_" else z

getLabelName :: Fun -> String
getLabelName = mkName ["Rule"] CamelCase

-- | Make a new entrypoint NT for an existing NT.

startSymbol :: String -> String
startSymbol = ("Start_" ++)
