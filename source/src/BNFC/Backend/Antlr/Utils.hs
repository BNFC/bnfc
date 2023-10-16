{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Antlr.Utils where

import Text.PrettyPrint.HughesPJ (Doc, text, vcat)
import Prelude hiding (Either, Left, Right)
import System.FilePath ((<.>), (</>))

import BNFC.CF (Fun)
import BNFC.Utils ( mkName, NameStyle(..), (+++))
import BNFC.Options as Options
import BNFC.Backend.Common.Makefile as MakeFile

getRuleName :: String -> String
getRuleName z = if z == "grammar" then z ++ "_" else z

getLabelName :: Fun -> String
getLabelName = mkName ["Rule"] CamelCase

-- | Make a new entrypoint NT for an existing NT.

startSymbol :: String -> String
startSymbol = ("Start_" ++)

dotG4 :: String -> String
dotG4 = (<.> "g4")

-- Left | Middle | Rigth
data Either3 a b c = L a | M b | R c

-- There are three variants of ANTLRv4 options:
-- "-OptName", "-OptName=OptValue", "-OptName Optvalue"
type OptionType = Either3 Bool String String

getAntlrOptions :: SharedOptions -> String
getAntlrOptions Options{..} = unwords $ map ("-" ++) parsedOpts
  where
    parsedOpts = getAntlrOptions'
      [ ("listener",    L listener)
      , ("no-listener", L $ not listener)
      , ("visitor",     L visitor)
      , ("no-visitor",  L $ not visitor)
      , ("Werror",      L wError)
      , ("Dlanguage",   M $ parseAntlrTarget dLanguage)
      , ("Xlog",        L xlog)
      , ("XdbgST",      L xDbgST)
      , ("XdbgSTWait",  L xDbgSTWait)
      , ("atn",         L atn)
      ]

getAntlrOptions' :: [(String, OptionType)] -> [String]
getAntlrOptions' [] = []
getAntlrOptions' (opt : opts) = case opt of
    (_, L False)         -> otherOpts
    (optName, L True)    -> optName : otherOpts
    (optName, M value)   -> (optName ++ "=" ++ value) : otherOpts
    (optName, R value)   -> (optName +++ value) : otherOpts
  where
    otherOpts = getAntlrOptions' opts

parseAntlrTarget :: AntlrTarget -> String
parseAntlrTarget Java    = "Java"
parseAntlrTarget CPP     = "Cpp"
parseAntlrTarget CSharp  = "CSharp"
parseAntlrTarget JS      = "JavaScript"
parseAntlrTarget TS      = "TypeScript"
parseAntlrTarget Dart    = "Dart"
parseAntlrTarget Python3 = "Python3"
parseAntlrTarget PHP     = "PHP"
parseAntlrTarget Go      = "Go"
parseAntlrTarget Swift   = "Swift"
