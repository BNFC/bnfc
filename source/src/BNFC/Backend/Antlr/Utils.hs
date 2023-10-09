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

dotG4 :: String -> String
dotG4 = (<.> "g4")

-- maybe should use instead of "getAntlrFlags"
getAntlrOptions :: SharedOptions -> String
getAntlrOptions Options{..} = unwords $ map ("-" ++) parsedOpts
  where
    parsedOpts = getAntlrOptions'
      [ ("listener",    Left listener)
      , ("no-listener", Left $ not listener)
      , ("visitor",     Left visitor)
      , ("no-visitor",  Left $ not visitor)
      , ("Werror",      Left wError)
      , ("Dlanguage",   Right $ parseAntlrTarget dLanguage)
      , ("Xlog",        Left xlog)
      ]

getAntlrOptions' :: [(String, Either Bool String)] -> [String]
getAntlrOptions' [] = []
getAntlrOptions' (opt : opts) = case opt of
    (_, Left False)     -> otherOpts
    (flag, Left True)   -> flag : otherOpts
    (flag, Right value) -> (flag ++ "=" ++ value) : otherOpts
  where
    otherOpts = getAntlrOptions' opts

parseAntlrTarget :: AntlrTarget -> String
parseAntlrTarget Java = "Java"
parseAntlrTarget CPP = "Cpp"
parseAntlrTarget CSharp = "CSharp"
parseAntlrTarget JS = "JavaScript"
parseAntlrTarget TS = "TypeScript"
parseAntlrTarget Dart = "Dart"
parseAntlrTarget Python3 = "Python3"
parseAntlrTarget PHP = "PHP"
parseAntlrTarget Go = "Go"
parseAntlrTarget Swift = "Swift"
