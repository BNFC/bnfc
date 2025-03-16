{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Scala Parser syntax
    Copyright (Scala) 2024  Author:  Juan Pablo Poittevin

    Description   : This module generates the Scala Parser Syntax
                    Using Scala Parser Combinator
    Author        : Juan Pablo Poittevin
    Created       : 30 September, 2024
-}

module BNFC.Backend.Scala.CFtoScalaParser (cf2ScalaParser) where

import Prelude hiding ((<>))

import qualified Data.Foldable as DF (toList)
import BNFC.Utils (symbolToName, unless)
import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Options
import BNFC.Backend.Common (unicodeAndSymbols)
import Data.List
import BNFC.Backend.Common.OOAbstract (cf2cabs, absclasses)
import Data.Map (Map, toList)
import BNFC.Backend.Common.NamedVariables (firstLowerCase, numVars)
import BNFC.Backend.Common.StrUtils (renderCharOrString)
import BNFC.Backend.Haskell.Utils (catToType)

cf2ScalaParser
  :: SharedOptions     
  -> CF     -- Grammar.
  -> Doc 
cf2ScalaParser Options{ lang } cf = vsep . concat $
  [ 
      []
    , imports lang
    , initWorkflowClass
    , map (nest 4) addExtraClasses
    , map (nest 4) getApplyFunction
    , map (nest 4) (getProgramFunction cf)
    , map (nest 4) getBlockFunction
    -- , map (nest 4) (addParserFunctions liters)
    -- , map (nest 4) (getKeywordParsers symbs)
    , strRules
    , endWorkflowClass
  ]
  where
    -- absc         = absclasses cabs
    -- cabs         = cf2cabs cf
    -- symbs        = unicodeAndSymbols cf
    -- rules        = ruleGroupsInternals cf
    -- cfKeywords   = reservedWords cf
    -- cfgSign      = signatureToString (cfgSignature cf)
    -- astData      = specialData cf
    -- astData      = getAbstractSyntax cf
    -- datasToPrint = getDatas astData
    -- parserCats   = allParserCats cf 
    strRules     = ruleGroupsCFToString (ruleGroups cf)

-- Función para convertir Signature en String sin position
-- signatureToString :: Map String (WithPosition Type) -> String
-- signatureToString sig =
--     intercalate "\n" [key ++ " -> " ++ show (wpThing wp) | (key, wp) <- toList sig]

-- dataToString :: Data -> String
-- dataToString (cat, []) = catToStr cat
-- dataToString (cat, cats) = "Main Cat: " ++ catToStr cat ++ " : " ++ strCatToString cats

-- strCatToString :: [(String, [Cat])] -> String
-- strCatToString ([]) = ""
-- strCatToString (ncat:[]) = " \n Sub Cat: " ++ fst ncat ++ " Is compose of: " ++ intercalate " " (map catToStr (snd ncat))
-- strCatToString (ncat:cats) = " \n  Sub Cat: " ++ fst ncat ++ " Is compose of: " ++ intercalate " " (map catToStr (snd ncat)) ++  strCatToString cats

-- getDatas :: [Data] -> [Doc]
-- getDatas datas = map (text . dataToString) datas

-- ruleToString :: Rule fun -> String
-- ruleToString (funRule) = ""
-- ruleToString (valRCat) = ""
-- ruleToString (rhsRule) = ""
-- ruleToString (internal) = ""

getSymbFromName :: String -> String
getSymbFromName s = 
  case symbolToName s of
  Just s -> s
  _ -> s

-- renderX :: String -> Doc
-- renderX sep' = "render" <> char sc <> parens (text sep)
--   where (sc, sep) = renderCharOrString sep'

-- basicFunName :: TokenCat -> String
-- basicFunName k
--   | k `elem` baseTokenCatNames = k
--   | otherwise                  = "Ident"

-- prPrintItem :: String -> Either (Cat, Doc) String -> String
-- prPrintItem pre = \case
--   Right t -> getSymbFromName t
--   Left (cat, nt) -> concat
--     [ "case "
--     -- Esta linea obtiene el TokenCat, y le aplica basicFunName, si no es un TokenCat, le aplica el dentCat $ normCat, es decir que lo maneja como un ListCat
--     -- esta obviando el CoercCat (Exp1, Exp2, etc)  
--     , maybe (identCat $ normCat cat) basicFunName $ maybeTokenCat cat
--     , "(", pre, render nt, ", ", show (precCat cat), ");"
--     ]


catToStrings :: [Either Cat String] -> [String]
catToStrings = map (\case
                  Left c -> show c
                  Right s -> s
                )


prPrintRule_ :: IsFun a => String -> Rul a -> [String]
prPrintRule_ _ (Rule _ _ items _) = map ((++ "()") . getSymbFromName) $ catToStrings items


prCoerciveRule :: Rule -> [String]
prCoerciveRule r@(Rule _ _ _ _)  = concat [
      [render type']
  ]
  where 
    type' = catToType id empty $ valCat r


prSubRule :: Rule -> [String]
prSubRule r@(Rule fun cat _ _) 
  -- | isCoercion fun = prCoerciveRule r
  | isCoercion fun = [""]
  | otherwise =
  [ 
    -- [ "def " ++ pre ++ ": Parser[" ++ fnm ++ "] = {"]
    "case " ++ intercalate " ~ " (prPrintRule_ pre r) ++  " => "
    ++ fnm ++ "(" ++ intercalate " , " (prPrintRule_ pre r) ++ ")"
  -- , [ "case " ++ show p ++ ") renderC(_R_PAREN);"
  --     , "}"
  --   ]
  -- , ["}"]
  ]
  where
    fnm = funName fun
    pre = firstLowerCase fnm


-- prRules :: Bool -> Rul a -> Doc
-- prRules functor = vsep . map prOne
--   where
--     prOne (_ , []      ) = empty -- nt has only internal use
--     prOne (nt, (p,a):ls) = vcat
--         [ hsep [ nt', "::", "{", if functor then functorType' nt else type' nt, "}" ]
--         , hang nt' 2 $ sep (pr ":" (p, a) : map (pr "|") ls)
--         ]
--       where
--         nt'          = text (identCat nt)
--         pr pre (p,a) = hsep [pre, text p, "{", text a , "}"]
--     type'            = catToType qualify empty
--     functorType' nt  = hcat ["(", qualify posType, ", ", type' nt, ")"]
--     qualify
--       | null absM = id
--       | otherwise = ((text absM <> ".") <>)
  
coerCatDefSign :: Cat -> Doc
coerCatDefSign cat = 
    text $ "def " ++ pre ++ ": Parser[" ++ sCat ++ "]"
  where
    sCat = render $ catToType id empty cat
    pre = firstLowerCase sCat

prSubRuleDoc :: Rule -> [Doc]
prSubRuleDoc r =  map text (prSubRule r)

rulesToString :: [Rule] -> [Doc]
rulesToString ([])   = [""]
rulesToString (r:[]) = prSubRuleDoc r
rulesToString (r:rs) = prSubRuleDoc r ++ rulesToString rs 

ruleGroupsCFToString :: [(Cat,[Rule])] -> [Doc]
ruleGroupsCFToString  ([])        =  [""]
ruleGroupsCFToString ((c, r):[] ) = [coerCatDefSign c] ++ [codeblock 4 (rulesToString r)]
ruleGroupsCFToString ((c, r):crs) = [coerCatDefSign c] ++ [codeblock 4 (rulesToString r)] ++ (ruleGroupsCFToString crs)


imports :: String -> [Doc]
imports name  = [
   text $ "package " ++ name ++ ".workflowtoken." ++ name ++ "Parser"
   , "import co.enear.parsercombinators.compiler.{Location, WorkflowParserError}"
   , "import co.enear.parsercombinators.lexer._"
   , "import scala.util.parsing.combinator.Parsers"
   , "import scala.util.parsing.input.{NoPosition, Position, Reader}"
  ]


addExtraClasses :: [Doc]
addExtraClasses = [
    "class WorkflowTokenReader(tokens: Seq[WorkflowToken]) extends Reader[WorkflowToken] {"
    , nest 4 "override def first: WorkflowToken = tokens.head"
    , nest 4  "override def atEnd: Boolean = tokens.isEmpty"
    , nest 4  "override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)"
    , nest 4  "override def rest: Reader[WorkflowToken] = new WorkflowTokenReader(tokens.tail)"
    , "}"
  ]


initWorkflowClass :: [Doc]
initWorkflowClass = [
      "object WorkflowParser extends Parsers {"
    , nest 4 "override type Elem = WorkflowToken"
    -- TODO: I REMOVED THIS LINE TO MAKE IT WORK, BUT WILL FAIL WITH GRAMMARS WHICH USE TABS/SPACES AS BLOCKS DEFINITION, SHOULD WE WORK ON THIS?
    -- , nest 4 "override def skipWhitespace = true"
    -- In case the lenguage use indentation for block creation, we should remove \n from the list of whiteSpace.
    -- , nest 4 $ text $ "override val whiteSpace = " ++ "\"" ++ "[\\t\\r\\f\\n]+\".r"
  ]

endWorkflowClass :: [Doc]
endWorkflowClass = [
    "}"
  ]

getApplyFunction :: [Doc]
getApplyFunction = [
     "def apply(tokens: Seq[WorkflowToken]): Either[WorkflowParserError, WorkflowAST] = {"
    , nest 4  "val reader = new WorkflowTokenReader(tokens)"
    , nest 4  "program(reader) match {"
    , nest 4    "case NoSuccess(msg, next) => Left(WorkflowParserError(Location(next.pos.line, next.pos.column), msg))"
    , nest 4    "case Success(result, next) => Right(result)"
    , nest 4  "}"
    ,  "}"
  ]



getProgramFunction :: CF -> [Doc]
getProgramFunction cf = [
    "def program: Parser[WorkflowAST] = positioned {"
    , nest 4 $ text $ "phrase(" ++ (intercalate " | " epsStr) ++ ")"
    ,"}"
  ]
  where
    epsStr = map show eps
    eps = DF.toList (allEntryPoints cf)


getBlockFunction :: [Doc]
getBlockFunction = [
  "def block: Parser[WorkflowAST] = positioned {"
  , nest 4  "rep1(statement) ^^ { case stmtList => stmtList reduceRight AndThen }"
  , "}"
  ]


getDoubleFunction :: Doc
getDoubleFunction = vcat [
      "def double: Parser[Double] = {"
    , nest 4 "\"[0-9]+.[0-9]+\".r ^^ {i => Double(i)}"
    , "}"
  ]


getLiteralsFunction :: Doc
getLiteralsFunction = vcat [
      "def string: Parser[STRING] = {"
    , nest 4 "\"\\\"[^\\\"]*\\\"\".r ^^ { str =>"
    , nest 6 "val content = str.substring(1, str.length - 1)"
    , nest 6 "STRING(content)"
    , nest 4 "}"
    , "}"
  ]

getLiteralFunction :: Doc
getLiteralFunction = vcat [
      "def char: Parser[CHAR] = {"
    , nest 4 "\"\\\'[^\\\']*\\\'\".r ^^ { str =>"
    , nest 6 "val content = str.substring(1, str.length - 1)"
    , nest 6 "CHAR(content)"
    , nest 4 "}"
    , "}"
  ]


getIndentationsFunction :: [Doc]
getIndentationsFunction = [
    "def indentation: Parser[INDENTATION] = {"
  , nest 4 "\"\\n[ ]*\".r ^^ { whitespace =>"
  , nest 6 "val nSpaces = whitespace.length - 1"
  , nest 6 "INDENTATION(nSpaces)"
  , nest 4 "}"
  , "}"
  ]


-- getSymbFromName :: String -> String
-- getSymbFromName s = 
--   case symbolToName s of
--   Just s -> s
--   _ -> s



-- getTokensFunction :: [String] -> [Doc]
-- getTokensFunction symbs = [
--      "def tokens: Parser[List[WorkflowToken]] = {" 
--     , nest 4 $ text $ "phrase(rep1( " ++ intercalate " | " (getListSymNames symbs) ++ "))"
--     -- next line is needed in case of indentation use
--     -- , nest 4 $ text $ "phrase(rep1(" ++ intercalate " | " (map (\s -> "\"" ++ s ++ "\"") symbs) ++ ")) ^^ { rawTokens =>"
--     -- , nest 8 "processIndent(rawTokens)"
--     , "}"
--   ]


-- getSymName :: String -> String
-- getSymName = toLowerString.getSymbFromName

-- getListSymNames :: [String] -> [String]
-- getListSymNames symbs = map getSymName symbs

-- getKeywordParsers :: [String] -> [Doc]
-- getKeywordParsers symbs = map getKeywordParser symbs


-- def plus          = positioned { "+"      ^^ (_ => PLUS())  }
-- getKeywordParser :: String -> Doc
-- getKeywordParser symb = text $ "def " ++ getSymName symb ++ " = positioned { \"" ++ toLowerString symb ++ "\" ^^ (_ =>"++ getSymbFromName symb ++"()) }"
-- def+= +^^ (_ =>+())

-- following functino must be added only in case of acepting indentation as block separetion
-- getProcessIndentFunction :: [Doc]
-- getProcessIndentFunction = [
--   "private def processIndent(tokens: List[WorkflowToken], indents: List[Int] = List(0)): List[WorkflowToken] = {"
--    nest 4 "tokens.headOption match {"

--       case Some(INDENTATION(spaces)) if spaces > indents.head =>
--         INDENT() :: processIndentations(tokens.tail, spaces :: indents)

--       // if there is a decrease, we pop from the stack until we have matched the new level and
--       // we produce a DEDENT for each pop
--       case Some(INDENTATION(spaces)) if spaces < indents.head =>
--         val (dropped, kept) = indents.partition(_ > spaces)
--         (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

--       // if the indentation level stays unchanged, no tokens are produced
--       case Some(INDENTATION(spaces)) if spaces == indents.head =>
--         processIndentations(tokens.tail, indents)

--       // other tokens are ignored
--       case Some(token) =>
--         token :: processIndentations(tokens.tail, indents)

--       // the final step is to produce a DEDENT for each indentation level still remaining, thus
--       // "closing" the remaining open INDENTS
--       case None =>
--         indents.filter(_ > 0).map(_ => DEDENT())

--     }
--   }
--   ]