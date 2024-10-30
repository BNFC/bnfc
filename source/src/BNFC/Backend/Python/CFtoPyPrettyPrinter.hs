
{-  
    BNF Converter: Python pretty-printer generator
    Copyright (C) 2024  Author: Bjorn Werner
    Based on CFtoCPrinter.hs, Copyright (C) 2004 Michael Pellauer
-}

module BNFC.Backend.Python.CFtoPyPrettyPrinter ( cf2PyPretty ) where
import Data.List     ( intercalate, nub )
import BNFC.CF 
import BNFC.Backend.Python.PyHelpers
import BNFC.Backend.Common.NamedVariables
import Text.PrettyPrint (Doc, render)
import Data.Either   (lefts)
import BNFC.Backend.Common.StrUtils
import qualified Data.List.NonEmpty as List1

-- | Used to create PrettyPrinter.py, that contains the functionality
--   to print the AST and the linearized tree.
cf2PyPretty :: String -> CF -> String
cf2PyPretty pkgName cf = unlines
  [ "from " ++ pkgName ++ ".Absyn import *"
  , ""
  , makePrintAST cf
  , ""
  , makeListDecons cf
  , ""
  , makeRenderC
  , ""
  , makeCoercCompare cf
  , ""
  , makeCompareFunc
  , ""
  , makeLinFunc cf
  ]


-- | Creates the print AST function.
makePrintAST :: CF -> String
makePrintAST cf = concat 
  [ "def printAST(ast: object) -> list:\n"
  , "  match ast:\n"
  , concat 
    [ ifUsedThen catInteger
      [ "    case Integer():"
      , "      return str(ast)"
      ]
    , ifUsedThen catDouble
      [ "    case Double():"
      , "      if ast.is_integer():"
      , "        return str(int(ast))"
      , "      else:"
      , "        return str(ast)"
      ]
    , ifUsedThen catString
      [ "    case String():"
      , "      return str(ast)"
      ]
    , ifUsedThen catChar
      [ "    case Char():"
      , "      return str(ast)"
      ]
    , ifUsedThen catIdent
      [ "    case Ident():"
      , "      return '\"' + str(ast) + '\"'"
      ]
    ]
  , if length (tokenNames cf) > 0
    then unlines 
      [ "    case (" ++ intercalate " | " (map (++"()") (tokenNames cf)) 
        ++ "):"
      , "      return '\"' + str(ast) + '\"'"
      ]
    else ""
  , "    case list():\n"
  , "      return '[' + ', '.join([printAST(a) for a in ast]) + ']'\n"
  , "\n"
  , "  if len(vars(ast)) > 0:\n"
  , "    return '(' + ast.__class__.__name__ + ' ' + ' '.join([printAST(vars(ast)[k]) for k in vars(ast) if k != '_ann_type']) + ')'\n"
  , "  else:\n"
  , "    return ast.__class__.__name__\n"
  ]
  where
    ifUsedThen :: TokenCat -> [String] -> String
    ifUsedThen cat ss
      | isUsedCat cf (TokenCat cat) = unlines ss
      | otherwise = ""


-- Creates deconstructors for all list categories.
makeListDecons :: CF -> String
makeListDecons cf = unlines $ map (makeListDecon cf) listCats
    where
        rules = cfgRules cf
        valCats = nub $ map valCat rules
        listCats = [c | c <- valCats, isList c]


-- Creates a deconstructor for some list category.
makeListDecon :: CF -> Cat -> String
makeListDecon cf c = concat 
  [ "def list" ++ name ++ "Decon(xs):\n"
  , oneRuleStr
  , nilRuleStr
  , consRuleStr
  , "\n"
  ]
  where
    name = show $ catOfList c
    listRulesForCat = rulesForCat cf c

    nilRule = case [r | r <- listRulesForCat, isNilFun r] of
      [] -> Nothing
      rs -> Just (head rs)
    oneRule = case [r | r <- listRulesForCat, isOneFun r] of
      [] -> Nothing
      rs -> Just (head rs)
    consRule = case [r | r <- listRulesForCat, isConsFun r] of
      [] -> Nothing
      rs -> Just (head rs)

    -- List rules are of the form:
    -- [C] ::= symbols.. C symbols.. [C]
    -- The production, in Python, is concatenated recursively:
    -- symbols.. + lin(xs[0]) + symbols.. + listCDecon(xs[1:]) + symbols..
    sentFormToArgs :: Int -> [Either Cat String] -> String
    sentFormToArgs _ [] = "[]"
    sentFormToArgs v (Right strOp:ecss) =
      "['" ++ escapeChars strOp ++ "'] + " ++ 
      sentFormToArgs v ecss
    sentFormToArgs v (Left _:ecss)
      | v == 0 = "lin(xs[0]) + " ++ sentFormToArgs (v+1) ecss
      | v == 1 = "list" ++ name ++ "Decon(xs[1:]) + " ++ 
      sentFormToArgs (v+1) ecss
      | otherwise = error "A list production can max have C and [C]."

    nilRuleStr = case nilRule of
      Nothing -> ""
      Just r -> unlines 
        [ "  if len(xs) == 0:"
        , "    return " ++ sentFormToArgs 0 (rhsRule r)
        ]

    oneRuleStr = case oneRule of
      Nothing -> ""
      Just r -> unlines 
        [ "  if len(xs) == 1:"
        , "    return " ++ sentFormToArgs 0 (rhsRule r)
        ]

    consRuleStr = case consRule of
      Nothing -> ""
      Just r -> "  return " ++ sentFormToArgs 0 (rhsRule r) ++ "\n"


-- | Creates the renderC function, which creates a string of a list of
--   strings, and inserts white-spaces to render the language in a C-like
--   manner.
makeRenderC :: String
makeRenderC = unlines 
  [ "def renderC(ss: list):"
  , "  def br(i):"
  , "    return '\\n' + '  ' * iLevel"
  , ""
  , "  def ident(i):"
  , "    return '  ' * iLevel"
  , ""
  , "  def removeTrailingWhitespace(tot):"
  , "    i = len(tot)"
  , "    while i > 0:"
  , "      if tot[i] == ' ':"
  , "        i -= 1"
  , "      else:"
  , "        break"
  , ""
  , "    return tot[:i]"
  , ""
  , "  def oneEmptyLine(tot):"
  , "    tot = tot.rstrip(' ')"
  , "    if len(tot) > 0 and tot[-1] != '\\n':"
  , "      tot += '\\n'"
  , "    tot += ident(iLevel)"
  , "    return tot"
  , ""
  , "  tot = ''"
  , "  iLevel = 0"
  , "  for i in range(len(ss)):"
  , "    s = ss[i]"
  , "    match s:"
  , "      case '{':"
  , "        tot = oneEmptyLine(tot)"
  , "        iLevel += 1"
  , "        tot += '{' + br(iLevel)"
  , "      case ('(' | '['):"
  , "        tot += s"
  , "      case (')' | ']'):"
  , "        tot = tot.rstrip()"
  , "        tot += s + ' '"
  , "      case '}':"
  , "        iLevel -= 1"
  , "        tot = oneEmptyLine(tot)"
  , "        tot += s + br(iLevel)"
  , "      case ',':"
  , "        tot = tot.rstrip()"
  , "        tot += s + ' '"
  , "      case ';':"
  , "        tot = tot.rstrip()"
  , "        tot += s + br(iLevel)"
  , "      case '':"
  , "        tot += ''"
  , "      case ' ':"
  , "        tot += s"
  , "      case _:"
  , "        tot += s + ' '"
  , ""
  , "  return tot"
  ]


-- Provides a mapping from a rule to its value category.
makeCoercCompare :: CF -> String
makeCoercCompare cf = concat 
  [ "cdict = {\n"
  , unlines (map (\(fs, cs) -> "  " ++ fs ++ " : '" ++ cs ++ "',") scs)
  , "}"
  ]
  where
    scs :: [(String, String)]
    scs = [(funName r, (show . wpThing . valRCat) r) | r <- cfgRules cf,
      not (isCoercion r), not (isNilCons r), not (isDefinedRule r)]


-- | Creates a function that attempts to figure out if
--   parentheses are required, for example:
--   1 + (2 * 3)
--   The precedence for the addition is low, say Exp, but the multiplication
--   has a higher precedence, say Exp1, so parantheses are needed.
makeCompareFunc :: String
makeCompareFunc = unlines 
  [ "def c(ast, cat: str) -> list:"
  , "  cl = ast.__class__"
  , "  if cl in cdict:"
  , "    clCat = cdict[cl]"
  , "    clCatAlphas = ''.join(filter(str.isalpha, clCat))"
  , "    catAlphas = ''.join(filter(str.isalpha, cat))"
  , "    clCatNums = ''.join(filter(str.isnumeric, clCat))"
  , "    catNums = ''.join(filter(str.isnumeric, cat))"
  , "    clCatNum = 0"
  , "    catNum = 0"
  , "    if clCatAlphas == catAlphas:"
  , "      if len(clCatNums) > 0:"
  , "        clCatNum = int(clCatNums)"
  , "      if len(catNums) > 0:"
  , "        catNum = int(catNums)"
  , "      if clCatNum < catNum:"
  , "        return ['('] + lin(ast) + [')']"
  , "  return lin(ast)"
  ]


-- | Returns the AST as a list of characters, which can be sent into the
--   renderC.function.
makeLinFunc :: CF -> String
makeLinFunc cf = unlines 
  [ "def lin(ast: object) -> list:"
  , "  match ast:"
  , concat 
    [ ifUsedThen catInteger 
      [ "    case Integer():"
      , "      return [str(ast)]"
      ]
    , ifUsedThen catDouble 
      [ "    case Double():"
      , "      if ast.is_integer():"
      , "        return [str(int(ast))]"
      , "      else:"
      , "        return [str(ast)]"
      ]
    , ifUsedThen catString 
      [ "    case String():"
      , "      return [ast, ' ']"
      ]
    , ifUsedThen catIdent 
      [ "    case Ident():"
      , "      return [ast]"
      ]
    , ifUsedThen catChar 
      [ "    case Char():"
      , "      return [ast]"
      ]
    ]
  , "    # skeleTokenCases:"
  , unlines skeleTokenCases
  , "    # skeleRuleCases:"
  , unlines skeleRuleCases
  , -- Deals with cases where the entrypoint is say [Stm] or [Exp], 
    -- with pattern matching on the first object in the list.
    "    case " ++ "list():"
  , "      if len(ast) == 0:"
  , "        return []"
  , "      else:"
  , "        match ast[0]:"
  , unlines listEntrypointCases
  , "          case _:"
  , "            raise Exception(ast[0].__class__.__name__, " ++
      "'unmatched ast[0]')"
  , "    case _:"
  , "      raise Exception(str(ast.__class__) + ' unmatched')"
  ]
  where
    -- Used to include standard literals, if needed.
    ifUsedThen :: TokenCat -> [String] -> String
    ifUsedThen cat ss
      | isUsedCat cf (TokenCat cat) = unlines ss
      | otherwise = ""

    -- Figures out the deliminators for the separators and terminators,
    -- to further process a deconstructed object that contains list(s).
    rules = [r | r <- cfgRules cf
      , not (isCoercion r)
      , not (isDefinedRule r)
      , not (isNilCons r)
      ]

    skeleTokenCases = map makeSkeleTokenCase (tokenNames cf)
    skeleRuleCases = map makeSkeleRuleCase rules

    catEntrypointsForLists = 
      [catOfList c | c <- (List1.toList . allEntryPoints) cf, isList c]
    
    -- The Haskell backend defaults to the production for the lowest
    -- precedence for lists that are defined. Like ``separator Exp1 ","``.
    lowestPrecListCats = [c | c <- catEntrypointsForLists, 
      precCat c == (minimum (map precCat 
            [c2 | c2 <- catEntrypointsForLists, normCat c == normCat c2]
          )
        )
      ]

    listEntrypointCases = 
        map (makeListEntrypointCase cf) lowestPrecListCats


-- | Creates cases that checks what class individual nodes might be, meaning
--   the rule names, or the token categories
makeListEntrypointCase :: CF -> Cat -> String
makeListEntrypointCase cf c = concat 
  [ "          case " ++ intercalate "|" constructors ++ ":\n"
  , "            return list" ++ show c ++ "Decon(ast)"
  ]
  where
    constructors = if isTokenCat c 
      then [show c ++ "()"]
      else map ((++ "()") . funName) 
        [ 
          r | r <- rulesForNormalizedCat cf (normCat c), 
          not (isCoercion r), 
          not (isDefinedRule r)
        ]


-- Creates a case for a user defined literal, which inherits str.
makeSkeleTokenCase :: String -> String
makeSkeleTokenCase tokenName = concat
  [ "    case " ++ tokenName ++ "():\n"
  , "      return [ast]"
  ]


-- | Creates a case for some rule, with the additional information of what
--   separator- and terminator-delimiters there are. 
makeSkeleRuleCase :: Rul RFun -> String
makeSkeleRuleCase rule = concat
  [ "    case " ++ fName ++ "(" ++ varNamesCommad ++ "):\n"
  , "      # " ++ (showEcss sentForm) ++ "\n"
  , "      return " ++ if (length args > 0)
    then (intercalate " + " args) 
    else "[]"
  ]
  where
    fName = wpThing (funRule rule)
    sentForm = rhsRule rule

    nvCats = numVars sentForm :: [Either (Cat, Doc) String]

    enumeratedVarNames = [render d | (c, d) <- lefts nvCats]

    varNamesCommad = if length enumeratedVarNames > 0 
      then addCommas (enumeratedVarNames ++ ["_ann_type"])
      else ""

    args = ecssAndVarsToList 
      sentForm 
      enumeratedVarNames


-- | Creates a list of a production with both terminals and non-terminals.
ecssAndVarsToList :: [Either Cat String] -> [String] -> [String]
ecssAndVarsToList [] _ = []
ecssAndVarsToList (Left c:ecss) (s:ss)
  | isList c = ["list" ++ name ++ "Decon(" ++ s ++ ")"] ++ 
  ecssAndVarsToList ecss ss
  | otherwise = ["c(" ++ s ++ ", '" ++ (show c) ++ "')"] ++ 
  ecssAndVarsToList ecss ss
  where
    name = show $ catOfList c
ecssAndVarsToList (Right strOp:ecss) ss = 
  ["['" ++ escapeChars strOp ++ "']"] ++ ecssAndVarsToList ecss ss

