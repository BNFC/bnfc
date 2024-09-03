{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Swift.Common where

import Text.PrettyPrint (Doc, text)
import qualified Data.Map as Map
import BNFC.CF
import qualified Data.Char as Char
import Data.Char (toLower)
import BNFC.Utils (mkName, NameStyle (OrigCase, MixedCase), mkNames)
import BNFC.Backend.Common.NamedVariables (getVars, firstUpperCase)


cat2SwiftClassName :: String -> Cat -> String
cat2SwiftClassName langName cat = str2SwiftClassName langName $ identCat $ normCat cat


-- Pick a class name that is appropriate for the Swift
str2SwiftClassName :: String -> String -> String
-- str2SwiftClassName langName str = upperFirst $ censorName langName str
str2SwiftClassName langName str = wrapIfNeeded $ upperFirst str

-- Pick a case name that is appropriate for the Swift
str2SwiftCaseName :: String -> String -> String
str2SwiftCaseName langName str = lowerFirst $ censorName langName str

-- Pick a class name that is appropriate for the Antlr
str2AntlrClassName :: String -> String
str2AntlrClassName str = upperFirst str


cat2SwiftType :: String -> Cat -> SwiftVarType
cat2SwiftType langName cat = toList (0, cat)
  where
    toList :: (Int, Cat) -> SwiftVarType
    toList (n, (ListCat name)) = toList (n + 1, name)
    toList (n, name) = 
      ( n
      , let n = catToStr $ normCat name
        in case (name2SwiftBuiltIn n) of 
          Just bn -> bn
          Nothing -> censor n )
    censor = censorName langName


cat2SwiftName :: String -> Cat -> String
cat2SwiftName langName cat = toList $ normCat cat
  where
    toList (ListCat name) = toList name ++ "List"
    toList name = censorName langName $ catToStr name


name2SwiftBuiltIn :: String -> Maybe String
name2SwiftBuiltIn name
  | name == "Integer" = Just "Int"
  | name == "Double" = Just "Double"
  | name == "Ident" = Just "String"
  | name == "String" = Just "String"
  | name == "Char" = Just "Character"
  | otherwise = Nothing


upperFirst :: [Char] -> [Char]
upperFirst [] = []
upperFirst (letter:rest) = Char.toUpper letter : rest


lowerFirst :: [Char] -> [Char]
lowerFirst [] = []
lowerFirst (letter:rest) = Char.toLower letter : rest


indent_ :: Int -> [String] -> [String]
indent_ n lines = map addSpaces lines
  where
    addSpaces :: String -> String
    addSpaces line = (replicate (2 * n) ' ') ++ line


indentString :: Int -> String -> String
indentString n line = addSpaces line
  where
    addSpaces :: String -> String
    addSpaces line = (replicate (2 * n) ' ') ++ line


-- The type of an instance variable.
-- Variable type, and its name
type SwiftVar = (SwiftVarType, SwiftVarName)


-- The type of a variable type in Swift.
-- The amount of nestings, and the underlying type name without precedence.
-- Example: List<List<Expr1>> is (2, Expr).
-- This helps to build the AST builder
type SwiftVarType = (Int, String)


-- The name of a variable.
-- the name generated from the type, 
-- and the number making this variable unique
type SwiftVarName = (String, Int)


-- Because of the different type representing variables, a different `getVars` is used.
getVars_ :: String -> [Cat] -> [SwiftVar]
getVars_ langName cats = 
  let variables = map toUnnamedVariable cats 
      namesMap = foldl countNames Map.empty variables
      scoreMap = Map.map addScore namesMap
      (_, vars) = foldl toSwiftVar (scoreMap, []) variables
  in vars
    where
      cat2SwiftName' = cat2SwiftName langName
      cat2SwiftType' = cat2SwiftType langName
      toUnnamedVariable cat = ((cat2SwiftType' cat), (cat2SwiftName' cat))
      countNames namesMap (_, name) = 
        let current = Map.findWithDefault 0 name namesMap
            next = 1 + current
        in Map.insert name next namesMap
      addScore n = (1, n)
      toSwiftVar (namesMap, vars) (vType, name) =
        case (Map.lookup name namesMap) of
          Nothing -> (
            namesMap, 
            vars ++ [(vType, (name, 0))])
          Just (seen, total) -> if total <= 1 
            then (
              namesMap, 
              vars ++ [(vType, (name, 0))])
            else (
              Map.insert name (seen + 1, total) namesMap, 
              vars ++ [(vType, (name, seen))])


-- From a SwiftVar build its string representation
buildVariableName :: SwiftVar -> String
buildVariableName (_, (name, num)) = lowerFirst appendNumber
  where
    appendNumber 
      | num <= 0 = name
      | otherwise = name ++ show num


-- From a SwiftVar make a name for the AST
buildVariableType :: SwiftVar -> String 
buildVariableType (vType, _) = buildVariableTypeFromSwiftType vType
  
buildVariableTypeFromSwiftType :: SwiftVarType -> String
buildVariableTypeFromSwiftType vType = unpack vType
  where 
    unpack (0, name) = name
    unpack (n, name) = "[" ++ unpack (n - 1, name) ++ "]"


checkBuiltIn :: String -> Bool
checkBuiltIn name = 
  (lowerFirst name) `elem` concatMap 
      (map lowerFirst) 
      [ builtIn, keywords ]


checkRegistered :: String -> Bool
checkRegistered name = 
  name `elem` (builtIn ++ keywords)


-- Prevent some type or variable name to be called as some already used type or keyword
censorName :: String -> String -> String
censorName langName name 
  | checkRegistered name = langName ++ upperFirst name
  | otherwise = name

wrapIfNeeded :: String -> String
wrapIfNeeded name
  | checkRegistered name = "`" ++ name ++ "`"
  | otherwise            = name 

taken :: [String]
taken = []

builtIn :: [String]
builtIn = [ "Int"
          , "Double"
          , "Float"
          , "String"
          , "Bool"
          , "Set"
          , "Void"
          , "Dictionary"
          , "Optional"
          , "Any" ]

keywords :: [String]
keywords = [ "abstract"
          , "as"
          , "assert"
          , "async"
          , "await"
          , "base"
          , "break"
          , "case"
          , "catch"
          , "class"
          , "const"
          , "continue"
          , "covariant"
          , "default"
          , "deferred"
          , "do"
          , "dynamic"
          , "else"
          , "enum"
          , "export"
          , "extends"
          , "extension"
          , "external"
          , "factory"
          , "false"
          , "final"
          , "finally"
          , "for"
          , "Function"
          , "get"
          , "hide"
          , "if"
          , "implements"
          , "import"
          , "in"
          , "interface"
          , "is"
          , "late"
          , "library"
          , "mixin"
          , "new"
          , "null"
          , "of"
          , "on"
          , "operator"
          , "part"
          , "required"
          , "rethrow"
          , "return"
          , "set"
          , "show"
          , "static"
          , "super"
          , "switch"
          , "sync"
          , "this"
          , "throw"
          , "true"
          , "try"
          , "type"
          , "typealias"
          , "var"
          , "void"
          , "when"
          , "with"
          , "while"
          , "yield" ]

-- from TS implementation

reservedKeywords :: [String]
reservedKeywords = builtIn ++ keywords

toMixedCase :: String -> String
-- toMixedCase = firstLowerCase . mkName reservedKeywords MixedCase
toMixedCase = firstUpperCase . mkName reservedKeywords MixedCase

-- | wrap string into single quotes.
wrapSQ :: String -> String
wrapSQ str = "'" ++ str ++ "'"

-- | indent string with N spaces.
indentStr :: Int -> String -> String
indentStr size = (replicate size ' ' ++)

mkTokenNodeName :: String -> String
mkTokenNodeName tokenName = tokenName ++ "Token"

-- | get variable names which will be used in node structure
-- for categories used in production rule.
getVarsFromCats :: [Cat] -> [String]
getVarsFromCats cats = mkNames ["type"] OrigCase normalizedVars
  where
    normalizedCats = map normCat cats
    indexedVars = getVars normalizedCats

    normalizeVar :: (String, Int) -> String
    normalizeVar (varName, idx) = map toLower varName ++ varNameSuffix
      where
        varNameSuffix = if idx == 0 then "" else show idx
    
    normalizedVars = map normalizeVar indexedVars

-- | indent string with N spaces and transform to Doc.
indent :: Int -> String -> Doc
indent size str = text (indentStr size str)

-- | get used tokens represented as cats
getAllTokenCats :: CF -> [Cat]
getAllTokenCats cf = map TokenCat (literals cf)

-- | get TS type names for all tokens
getAllTokenTypenames :: CF -> [String]
getAllTokenTypenames cf = map catToSwiftType (getAllTokenCats cf)

catToSwiftType :: Cat -> String
catToSwiftType (ListCat c) = "[" ++ catToSwiftType c ++ "]"
catToSwiftType (TokenCat c) = toMixedCase (c ++ "Token")
catToSwiftType cat = toMixedCase (catToStr cat)