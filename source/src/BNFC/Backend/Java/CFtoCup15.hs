{-
    BNF Converter: Java 1.5 Cup Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Description   : This module generates the CUP input file. It
                    follows the same basic structure of CFtoHappy.

    Author        : Michael Pellauer
                    Bjorn Bringert

    Created       : 26 April, 2003
    Modified      : 5 Aug, 2004

-}

module BNFC.Backend.Java.CFtoCup15 ( cf2Cup ) where

import Data.List (intercalate)

import BNFC.CF
import BNFC.Options (RecordPositions(..))
import BNFC.Utils ( (+++) )

import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Java.Utils            ( getRuleName )

type Rules   = [(NonTerminal,[(Pattern,Action)])]
type Pattern = String
type Action  = String
type MetaVar = String

--The environment comes from the CFtoJLex
cf2Cup :: String -> String -> CF -> RecordPositions -> KeywordEnv -> String
cf2Cup packageBase packageAbsyn cf rp env = unlines
    [ header
    , declarations packageAbsyn (allParserCats cf)
    , tokens env
    , specialToks cf
    , specialRules cf
    , prEntryPoint cf
    , prRules (rulesForCup packageAbsyn cf rp env)
    ]
  where
    header :: String
    header = unlines
      [ "// -*- Java -*- Parser definition for use with Java Cup"
      , "package" +++ packageBase ++ ";"
      , ""
      , "action code {:"
      , "public java_cup.runtime.ComplexSymbolFactory.Location getLeftLocation("
      , "    java_cup.runtime.ComplexSymbolFactory.Location ... locations) {"
      , "  for (java_cup.runtime.ComplexSymbolFactory.Location l : locations) {"
      , "    if (l != null) {"
      , "      return l;"
      , "    }"
      , "  }"
      , "  return null;"
      , "}"
      , ":}"
      , "parser code {:"
      , parseMethod packageAbsyn (firstEntry cf)
      , "public void syntax_error(java_cup.runtime.Symbol cur_token)"
      , "{"
      , "  report_error(\"Syntax Error, trying to recover and continue"
        ++ " parse...\", cur_token);"
      , "}"
      , ""
      , "public void unrecovered_syntax_error(java_cup.runtime.Symbol "
        ++ "cur_token) throws java.lang.Exception"
      , "{"
      , "  throw new Exception(\"Unrecoverable Syntax Error\");"
      , "}"
      , ""
      , ":}"
      ]


-- peteg: FIXME JavaCUP can only cope with one entry point AFAIK.
prEntryPoint :: CF -> String
prEntryPoint cf = unlines ["", "start with " ++ identCat (firstEntry cf) ++ ";", ""]
--                  [ep]  -> unlines ["", "start with " ++ ep ++ ";", ""]
--                  eps   -> error $ "FIXME multiple entry points." ++ show eps

--This generates a parser method for each entry point.
parseMethod :: String -> Cat -> String
parseMethod packageAbsyn cat = unlines
             [ "  public" +++ packageAbsyn ++ "." ++ dat +++ "p" ++ cat' ++ "()"
                 ++ " throws Exception"
             , "  {"
             , "    java_cup.runtime.Symbol res = parse();"
             , "    return (" ++ packageAbsyn ++ "." ++ dat ++ ") res.value;"
             , "  }"
             ]
    where
    dat  = identCat (normCat cat)
    cat' = identCat cat

--non-terminal types
declarations :: String -> [Cat] -> String
declarations packageAbsyn ns = unlines (map (typeNT packageAbsyn) ns)
 where
   typeNT _nm nt = "nonterminal" +++ packageAbsyn ++ "."
                    ++ identCat (normCat nt) +++ identCat nt ++ ";"

--terminal types
tokens :: KeywordEnv -> String
tokens ts = unlines (map declTok ts)
 where
  declTok (s,r) = "terminal" +++ r ++ ";    //   " ++ s

specialToks :: CF -> String
specialToks cf = unlines
  [ ifC catString  "terminal String _STRING_;"
  , ifC catChar    "terminal Character _CHAR_;"
  , ifC catInteger "terminal Integer _INTEGER_;"
  , ifC catDouble  "terminal Double _DOUBLE_;"
  , ifC catIdent   "terminal String _IDENT_;"
  ]
   where
    ifC cat s = if isUsedCat cf (TokenCat cat) then s else ""

specialRules:: CF -> String
specialRules cf =
    unlines ["terminal String " ++ name ++ ";" | name <- tokenNames cf]

--The following functions are a (relatively) straightforward translation
--of the ones in CFtoHappy.hs
rulesForCup :: String -> CF -> RecordPositions -> KeywordEnv -> Rules
rulesForCup packageAbsyn cf rp env = map mkOne $ ruleGroups cf where
  mkOne (cat,rules) = constructRule packageAbsyn cf rp env rules cat

-- | For every non-terminal, we construct a set of rules. A rule is a sequence of
-- terminals and non-terminals, and an action to be performed.
constructRule :: String -> CF -> RecordPositions -> KeywordEnv -> [Rule] -> NonTerminal
    -> (NonTerminal,[(Pattern,Action)])
constructRule packageAbsyn cf rp env rules nt =
    (nt, [ (p, generateAction packageAbsyn nt (funName $ funRule r) (revM b m) b rp)
          | r0 <- rules,
          let (b,r) = if isConsFun (funRule r0) && elem (valCat r0) revs
                          then (True, revSepListRule r0)
                          else (False, r0)
              (p,m) = generatePatterns env r])
 where
   revM False = id
   revM True  = reverse
   revs       = cfgReversibleCats cf

-- Generates a string containing the semantic action.
generateAction :: String -> NonTerminal -> Fun -> [MetaVar]
               -> Bool   -- ^ Whether the list should be reversed or not.
                         --   Only used if this is a list rule.
               -> RecordPositions   -- ^ Record line and column info.
               -> Action
generateAction packageAbsyn nt fun ms rev rp
    | isNilFun f      = "RESULT = new " ++ c ++ "();"
    | isOneFun f      = "RESULT = new " ++ c ++ "(); RESULT.addLast("
                           ++ p_1 ++ ");"
    | isConsFun f     = "RESULT = " ++ p_2 ++ "; "
                           ++ p_2 ++ "." ++ add ++ "(" ++ p_1 ++ ");"
    | isCoercion f    = "RESULT = " ++ p_1 ++ ";"
    | isDefinedRule f = "RESULT = " ++ packageAbsyn ++ "Def." ++ sanitize f
                        ++ "(" ++ intercalate "," ms ++ ");"
    | otherwise       = "RESULT = new " ++ c
                  ++ "(" ++ intercalate "," ms ++ ");" ++ lineInfo
   where
     sanitize = getRuleName
     f   = funName fun
     c   = packageAbsyn ++ "." ++
           if isNilFun f || isOneFun f || isConsFun f
             then identCat (normCat nt) else f
     p_1 = ms !! 0
     p_2 = ms !! 1
     add = if rev then "addLast" else "addFirst"
     lineInfo =
        if rp == RecordPositions
          then case ms of
            [] -> "\n((" ++ c ++ ")RESULT).line_num = -1;" ++
                  "\n((" ++ c ++ ")RESULT).col_num = -1;" ++
                  "\n((" ++ c ++ ")RESULT).offset = -1;"
            _  -> "\njava_cup.runtime.ComplexSymbolFactory.Location leftLoc = getLeftLocation(" ++
                  intercalate "," (map (++"xleft") ms) ++ ");" ++
                  "\nif (leftLoc != null) {" ++
                  "\n  ((" ++ c ++ ")RESULT).line_num = leftLoc.getLine();" ++
                  "\n  ((" ++ c ++ ")RESULT).col_num = leftLoc.getColumn();" ++
                  "\n  ((" ++ c ++ ")RESULT).offset = leftLoc.getOffset();" ++
                  "\n} else {" ++
                  "\n  ((" ++ c ++ ")RESULT).line_num = -1;" ++
                  "\n  ((" ++ c ++ ")RESULT).col_num = -1;" ++
                  "\n  ((" ++ c ++ ")RESULT).offset = -1;" ++
                  "\n}"
          else ""


-- | Generate patterns and a set of metavariables indicating
-- where in the pattern the non-terminal.
--
-- >>> generatePatterns [] (npRule "myfun" (Cat "A") [] Parsable)
-- (" /* empty */ ",[])
--
-- >>> generatePatterns [("def", "_SYMB_1")] (npRule "myfun" (Cat "A") [Right "def", Left (Cat "B")] Parsable)
-- ("_SYMB_1:p_1 B:p_2 ",["p_2"])

generatePatterns :: KeywordEnv -> Rule -> (Pattern,[MetaVar])
generatePatterns env r = case rhsRule r of
    []  -> (" /* empty */ ", [])
    its -> (mkIt 1 its, metas its)
 where
    mkIt _ [] = []
    mkIt n (i:is) =
      case i of
        Left c -> c' ++ ":p_" ++ show (n :: Int) +++ mkIt (n+1) is
          where
              c' = case c of
                  TokenCat "Ident"   -> "_IDENT_"
                  TokenCat "Integer" -> "_INTEGER_"
                  TokenCat "Char"    -> "_CHAR_"
                  TokenCat "Double"  -> "_DOUBLE_"
                  TokenCat "String"  -> "_STRING_"
                  _ -> identCat c
        Right s -> case lookup s env of
            Just x  -> (x ++ ":p_" ++ show (n :: Int)) +++ mkIt (n+1) is
            Nothing -> mkIt n is
    metas its = ["p_" ++ show i | (i,Left _) <- zip [1 :: Int ..] its]

-- We have now constructed the patterns and actions,
-- so the only thing left is to merge them into one string.
prRules :: Rules -> String
prRules []                    = []
prRules ((_ , []      ) : rs) = prRules rs --internal rule
prRules ((nt, (p,a):ls) : rs) =
    unwords [ nt', "::=", p, "{:", a, ":}", '\n' : pr ls ] ++ ";\n" ++ prRules rs
  where
    nt' = identCat nt
    pr []           = []
    pr ((p,a):ls)   = unlines [ unwords [ "  |", p, "{:", a , ":}" ] ] ++ pr ls
