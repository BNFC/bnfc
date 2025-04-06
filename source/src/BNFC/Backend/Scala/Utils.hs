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
    wrapList, safeHeadString, scalaReserverWords, safeCatName, isLeft, getRHSCats, isSpecialCat, generateClassSignature,
    getSymbFromName, catToStrings, getFunName, hasTokenCat, safeRefCatName, inspectListRulesByCategory, isListCat
) where
import BNFC.CF
import Data.Map hiding (map, filter)
import BNFC.Backend.Common.NamedVariables (firstLowerCase)
import System.Directory.Internal.Prelude (fromMaybe)
import BNFC.Utils (symbolToName)
import Data.Char (toUpper)
import Text.PrettyPrint
import BNFC.PrettyPrint
import Data.List (isSuffixOf)
import GHC.OldList (intercalate)
import BNFC.Utils ((+++))


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
      ("def"  , "_def")
    , ("val"  , "_val")
    , ("var"  , "_var")
    , ("class", "_class")
    , ("object", "_object")
    , ("trait", "_trait")
    , ("extends", "_extends")
    , ("with", "_with")
    , ("case", "_case")
    , ("sealed", "_sealed")
    , ("abstract", "_abstract")
    , ("final", "_final")
    , ("override", "_override")
    , ("implicit", "_implicit")
    , ("lazy", "_lazy")
    , ("private", "_private")
    , ("protected", "_protected")
    , ("public", "_public")
    , ("import", "_import")
    , ("package", "_package")
    , ("return", "_return")
    , ("if", "_if")
    , ("else", "_else")
    , ("while", "_while")
    , ("for", "_for")
    , ("do", "_do")
    , ("match", "_match")
  ]

-- | Safe version of tail that returns an empty list for an empty list
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs 

-- | Safe version of head that returns an empty list for an empty list
safeHeadString :: [String] -> String
safeHeadString []     = ""
safeHeadString (x:_) = x

safeCatName :: Cat -> String
safeCatName cat = fromMaybe notSafeScalaCatName (scalaReserverWords notSafeScalaCatName)
  where
    notSafeScalaCatName = case cat of
      ListCat innerCat -> firstUpperCase (safeCatName innerCat) 
      _                -> firstLowerCase $ show cat


safeRefCatName :: Cat -> String
safeRefCatName cat = fromMaybe notSafeScalaCatName (scalaReserverWords notSafeScalaCatName)
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

-- | Generate the class signature
generateClassSignature :: Rule -> Bool-> String
generateClassSignature (Rule fun _ rhs _) withParams = className ++ "(" ++ params ++ ")"
  where
    -- Function to format parameters based on whether they are Cat or String
    catParams :: Either Cat String -> String
    catParams (Left c)  = formatParamType c
    catParams (Right _) = "WorkflowAST"

    className = funName fun
    params = if withParams then intercalate ", " $ zipWith (\x y -> x ++ ":" +++ y) (generateVarsList filteredRhs) (map catParams filteredRhs) else []
    filteredRhs = filter isLeft rhs


-- | Format a parameter with its type
formatParamType :: Cat -> String
formatParamType cat =
  let baseCat = unwrapListCat cat  -- Extraemos el TokenCat base
  in if baseCat `elem` BNFC.CF.baseTokenCatNames
       then case baseTypeToScalaType baseCat of
              Just s  -> wrapList cat s
              Nothing -> "String"  -- Default a "String"
       else wrapList cat "WorkflowAST"  -- Si no es baseTokenCat, usar WorkflowAST


-- | Gived a list of rhs, return the list vars in safe strings
-- | so for the EAdd it will return: ["exp", "PLUS()", "exp"]
rhsToSafeStrings :: Rule -> [String]
rhsToSafeStrings rule@(Rule _ _ rhs _) = Prelude.map (\case
        Left c -> safeCatName $ normCat c
        Right s -> case symbolToName s of
              Just s' -> s' ++ "()"
              Nothing -> generateClassSignature rule False
      ) rhs

-- | Get all the Left Cat of the rhs of a rule
getRHSCats :: [Either Cat String] -> [Cat]
getRHSCats rhs = [c | Left c <- rhs]

-- | Get the function name for a rule
getFunName :: Rule -> String
getFunName (Rule fun _ _ _) = wpThing fun

-- | Check if a rule contains a specific token category
hasTokenCat :: TokenCat -> Rule -> Bool
hasTokenCat token (Rule _ _ rhs _) = TokenCat token `elem` [c | Left c <- rhs]

isSpecialCat :: Cat -> Bool
isSpecialCat (TokenCat cat) = cat `elem` baseTokenCatNames
isSpecialCat _ = False


-- Function to generate a list of Doc from a (Cat, [Rule])
inspectRulesByCategory :: (Cat, [Rule]) -> [Doc]
inspectRulesByCategory (cat, rules) =
  [text "Category:" <+> pretty cat] ++
  [text "Rules:" $$ nest 2 (vcat (Prelude.map prettyRule rules))]
  where
    -- Pretty-print a single Rule
    prettyRule (Rule fun valCat rhs internal) =
      text "Function:" <+> pretty fun $$
      text "Value Category:" <+> pretty valCat $$
      text "RHS:" <+> text (show rhs) $$
      text "Internal:" <+> text (internalRuleToStrings internal)

internalRuleToStrings :: InternalRule -> String
internalRuleToStrings Internal = "Internal"
internalRuleToStrings Parsable = "Parsable"


-- | Convert a list of rules to a list of strings
inspectListRulesByCategory :: [(Cat, [Rule])] -> [Doc]
inspectListRulesByCategory rulesByCat = concatMap inspectRulesByCategory rulesByCat

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
      | "()" `isSuffixOf` n = n : disamb (n:ns1) ns2
      | n `elem` (ns1 ++ ns2) = let i = length (Prelude.filter (==n) ns1) + 1
                               in (n ++ show i) : disamb (n:ns1) ns2
      | otherwise = n : disamb (n:ns1) ns2
    disamb _ [] = []