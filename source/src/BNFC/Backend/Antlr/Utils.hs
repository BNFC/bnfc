{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Antlr.Utils where

import System.FilePath ((<.>))

import BNFC.CF (Fun)
import BNFC.Utils ( mkName, NameStyle(..))
import BNFC.Options as Options

getRuleName :: String -> String
getRuleName z = if z == "grammar" then z ++ "_" else z

getLabelName :: Fun -> String
getLabelName = mkName ["Rule"] CamelCase

-- | Make a new entrypoint NT for an existing NT.

startSymbol :: String -> String
startSymbol = ("Start_" ++)

getAntlrFlags :: SharedOptions -> String
getAntlrFlags Options{..} = unwords $ getFlags
  [ ("no-listener", not listener)
  , ("visitor", visitor)
  , ("Werror", wError)
  ]

getFlags :: [(String, Bool)] -> [String]
getFlags (x : xs) = case x of
  (flag, True) -> ("-" ++ flag) : getFlags xs
  (_, False)   -> getFlags xs

getFlags [] = []

dotG4 :: String -> String
dotG4 = (<.> "g4")
