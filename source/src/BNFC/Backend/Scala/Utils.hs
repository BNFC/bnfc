{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Utils
    Copyright (Scala) 2024  Author:  Juan Pablo Poittevin, Guillermo Poladura

    Description   : This module is a helper for Scala backend
    Author        : Juan Pablo Poittevin, Guillermo Poladura
    Created       : 30 September, 2024
-}

module BNFC.Backend.Scala.Utils (
    generateVarsList, unwrapListCat, baseTypeToScalaType, safeTail, rhsToSafeStrings, disambiguateNames, safeCatToStrings,
    wrapList, safeHeadString, scalaReserverWords, safeCatName, isLeft, getRHSCats, isSpecialCat, firstUpperCase, safeHeadChar,
    getSymbFromName, catToStrings, getFunName, hasTokenCat, isListCat, disambiguateTuples, getListSeparator,
    wildCardSymbs, mapManualTypeMap, scalaBNFCReserverWords, applyToRepeated, isCoercionRule, isCoercionCategory, isSymbol,
    getTerminalFromListRules
) where
import BNFC.CF
import Data.Map
import BNFC.Backend.Common.NamedVariables (firstLowerCase)
import System.Directory.Internal.Prelude (fromMaybe)
import BNFC.Utils (symbolToName)
import Data.Char (toUpper)
import Data.List (isSuffixOf, find)
import Data.Maybe (isJust)



generateVarsList :: [a] -> [String]
generateVarsList xs = zipWith (\_ (i :: Int) -> "var" ++ show i) xs [1..]

unwrapListCat :: Cat -> TokenCat
unwrapListCat (TokenCat c)    = c
unwrapListCat (ListCat lc)    = unwrapListCat lc
unwrapListCat (CoercCat cc _) = cc
unwrapListCat (Cat s)         = s

wrapList :: Cat -> String -> [Char]
wrapList cat s = case cat of
                ListCat _ -> "List[" ++ s ++ "]"
                _         -> s

-- | Safe version of tail that returns an empty list for an empty list
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs 

-- | Safe version of head that returns an empty list for an empty list
safeHeadString :: [String] -> String
safeHeadString []     = ""
safeHeadString (x:_) = x

-- | Safe version of head that returns an empty list for an empty list
safeHeadChar :: [Char] -> Char
safeHeadChar []     = ' '
safeHeadChar (x:_) = x

safeCatName :: Cat -> String
safeCatName cat = fromMaybe notSafeScalaCatName (scalaReserverWords notSafeScalaCatName)
  where
    notSafeScalaCatName = case cat of
      ListCat innerCat -> firstUpperCase (safeCatName innerCat) 
      _                -> firstLowerCase $ show cat


firstUpperCase :: String -> String
firstUpperCase []     = []
firstUpperCase (x:xs) = toUpper x : xs

-- | Get a symbol name from a string
getSymbFromName :: String -> String
getSymbFromName s = 
  case symbolToName s of
    Just s -> s ++ "()"
    _ -> s

wildCardSymbs :: String -> String
wildCardSymbs s = 
  case symbolToName s of
    Just _ -> "_"
    _ -> s

-- | Convert a category or string to its string representation
safeCatToStrings :: [Either Cat String] -> [String]
safeCatToStrings = Prelude.map (\case
              Left c -> safeCatName c
              Right s -> s
            )


-- | Convert a category or string to its string representation
catToStrings :: [Either Cat String] -> [String]
catToStrings = Prelude.map (\case
              Left c -> show c
              Right s -> s
            )

-- | Gived a list of rhs, return the list vars in safe strings
-- | so for the EAdd it will return: ["exp", "PLUS()", "exp"]
rhsToSafeStrings :: [Either Cat String] -> [String]
rhsToSafeStrings = Prelude.map (\case
              Left c -> safeCatName $ normCat c
              Right s -> case symbolToName s of
                          Just s' -> s' ++ "()"
                          Nothing -> (Prelude.map toUpper s) ++ "()"
            )
-- | Get all the Left Cat of the rhs of a rule
getRHSCats :: [Either Cat String] -> [Cat]
getRHSCats rhs = [c | Left c <- rhs]

getTerminalFromListRules :: [Rule] -> Maybe String
getTerminalFromListRules rules = (find (not . (== "")) terminals)
  where
    terminals = Prelude.concatMap (\rule ->
        Prelude.map (\rhs -> case rhs of
              Right s  -> fromMaybe "" $ symbolToName s
              _        -> ""  
              ) (rhsRule rule)
      
      ) rules

-- | Get the function name for a rule
getFunName :: Rule -> String
getFunName (Rule fun _ _ _) = wpThing fun

-- | Check if a rule contains a specific token category
hasTokenCat :: TokenCat -> Rule -> Bool
hasTokenCat token (Rule _ _ rhs _) = TokenCat token `elem` [c | Left c <- rhs]

isSpecialCat :: Cat -> Bool
isSpecialCat (TokenCat cat) = cat `elem` specialCatsP
isSpecialCat _ = False

isSymbol :: String -> Bool
isSymbol = isJust . symbolToName


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isListCat :: Cat -> Bool
isListCat (ListCat _) = True
isListCat _ = False

-- | Make variable names unique by adding numbers to duplicates
disambiguateNames :: [String] -> [String]
disambiguateNames = disamb []
  where
    disamb ns1 (n:ns2)
      | n == "_" = n : disamb (n:ns1) ns2
      | "()" `isSuffixOf` n = n : disamb (n:ns1) ns2
      | n `elem` (ns1 ++ ns2) = let i = length (Prelude.filter (==n) ns1) + 1
                               in (n ++ show i) : disamb (n:ns1) ns2
      | otherwise = n : disamb (n:ns1) ns2
    disamb _ [] = []

disambiguateTuples :: [(String, String)] -> [(String, String)]
disambiguateTuples tuples =
  let (names, values) = unzip tuples
      newNames = disambiguateNames names
  in zip newNames values

-- | applies a function only to the first occurrence of a repetead element of the list
-- | example: applyToRepeated (++ "1") ["a", "b", "a", "c"]
-- | ["a", "b", "a1", "c"]
applyToRepeated :: Eq a => (a -> a) -> [a] -> [a]
applyToRepeated f xs = apply [] xs
  where
    apply _ [] = []
    apply seen (y:ys)
      | y `elem` seen = f y : ys 
      | otherwise     = y : apply (y:seen) ys

-- | Check if a rule is a coercion rule
isCoercionRule :: Rule -> Bool
isCoercionRule (Rule fun _ _ _) = isCoercion fun

isCoercionCategory :: Cat -> Bool
isCoercionCategory (CoercCat _ _) = True
isCoercionCategory _ = False

getListSeparator :: CF -> Rule -> Maybe Symbol
getListSeparator cf (Rule _ cat _ _) =
  case normCat (wpThing cat) of
    ListCat baseCat ->
      let (_, layoutKeywords, _) = layoutPragmas cf
      in Prelude.lookup (show (normCat baseCat)) layoutKeywords >>= Just . listSep
    _ -> Nothing


  -- | Convert base LBNF type to Scala type
mapManualTypeMap :: String -> Maybe String
mapManualTypeMap = (`Data.Map.lookup` manualTypeMap)

-- | Map from base LBNF Type to scala Type
manualTypeMap :: Map String String
manualTypeMap = fromList manualTypesMap

-- | Scala types mapping
manualTypesMap :: [(String, String)]
manualTypesMap =
  [ ("INTEGER"  , "PINTEGER")
  , ("STRING"   , "PSTRING")
  , ("DOUBLE"   , "PDOUBLE")
  , ("CHAR"   , "PCHAR")
  , ("IDENT"   , "PIDENT")
  ]


-- | There are some words used in the Scala parser combinator code generated by BNFC
scalaBNFCReserverWords :: String -> Maybe String
scalaBNFCReserverWords = (`Data.Map.lookup` baseScalaBNFCReserverWordsMap)

-- | Map from base LBNF Type to scala Type
baseScalaBNFCReserverWordsMap :: Map String String
baseScalaBNFCReserverWordsMap = fromList scalaBNFCReserverWordsMap

-- | Scala types mapping
scalaBNFCReserverWordsMap :: [(String, String)]
scalaBNFCReserverWordsMap =
  [ ("string"  , "pstring")
  , ("integer"   , "pinteger")
  , ("double"   , "pdouble")
  , ("char"   , "pchar")
  , ("program"   , "internal_program")
  , ("apply"   , "internal_apply")
  ]


-- | Convert base LBNF type to Scala type
baseTypeToScalaType :: String -> Maybe String
baseTypeToScalaType = (`Data.Map.lookup` baseTypeMap)

-- | Map from base LBNF Type to scala Type
baseTypeMap :: Map String String
baseTypeMap = fromList scalaTypesMap

-- | Scala types mapping
scalaTypesMap :: [(String, String)]
scalaTypesMap =
  [ ("Integer"  , "Int")
  , ("String"   , "String")
  , ("Double"   , "Double")
  , ("Ident"   , "String")
  ]

scalaReserverWords :: String -> Maybe String
scalaReserverWords = (`Data.Map.lookup` reserverWordsMap)

-- | Map from base LBNF Type to scala Type
reserverWordsMap :: Map String String
reserverWordsMap = fromList wordsMap

-- | Scala reserverd words mapping
wordsMap :: [(String, String)]
wordsMap =
  [ 
      ("def"  , "pdef")
    , ("val"  , "pval")
    , ("var"  , "pvar")
    , ("class", "pclass")
    , ("type", "ptype")
    , ("object", "pobject")
    , ("trait", "ptrait")
    , ("extends", "pextends")
    , ("with", "pwith")
    , ("case", "pcase")
    , ("sealed", "psealed")
    , ("abstract", "pabstract")
    , ("final", "pfinal")
    , ("override", "poverride")
    , ("implicit", "pimplicit")
    , ("lazy", "plazy")
    , ("private", "pprivate")
    , ("protected", "pprotected")
    , ("public", "ppublic")
    , ("import", "pimport")
    , ("package", "ppackage")
    , ("return", "preturn")
    , ("if", "pif")
    , ("else", "pelse")
    , ("while", "pwhile")
    , ("for", "pfor")
    , ("do", "pdo")
    , ("match", "pmatch")
    , ("try", "pttry")
    , ("catch", "pcatch")
    , ("finally", "pfinally")
    , ("throw", "pthrow")
    , ("true", "ptrue")
    , ("false", "pfalse")
    , ("apply", "papply")
    , ("Int", "pInt")
    , ("String", "pString")
    , ("program", "pprogram")
  ]
