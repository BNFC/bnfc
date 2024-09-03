{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Swift.CFtoSwiftBuilder (cf2SwiftBuilder) where

import Data.Bifunctor (Bifunctor(second))
import Data.List (intercalate, intersperse)
import Data.Maybe (mapMaybe)

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)

import BNFC.Utils ((+++), camelCase_)
import BNFC.CF
import BNFC.Backend.Swift.Common
import BNFC.Options (SharedOptions (lang))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (antlrRuleLabel, makeLeftRecRule)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)

type RuleData = (Cat, [(String, SentForm)])

cf2SwiftBuilder :: CF -> SharedOptions -> Doc
cf2SwiftBuilder cf opts = vcat $ intersperse (text "")
    [ importDecls
    , errorsDecl
    , tokenDecls
    , buildFnDecls
    ]
  where
    language = lang opts
    
    importDecls :: Doc
    importDecls = vcat
        [ "import Foundation"
        , "import Antlr4"
        ]

    errorsDecl = buildErrors
    tokenDecls = vcat $ intersperse (text "") buildTokensFuns
    buildFnDecls = vcat $ intersperse (text "") buildFuns

    buildFuns = map (mkBuildFunction language) datas 
    buildTokensFuns = map mkBuildTokenFunction allTokenCats

    allTokenCats = getAllTokenCats cf
    datas = cfToGroups cf

buildErrors :: Doc
buildErrors = vcat
  [ "enum BuildError: Error {"
  , indent 2 "case UnexpectedParseContext(String)"
  , "}"
  ]

mkThrowErrorStmt :: Cat -> String
mkThrowErrorStmt cat = "throw BuildError.UnexpectedParseContext(\"Error: ctx should be an instance of" +++ camelCase_ (identCat cat) ++ "Context" ++ "\")"

-- | generates function code for building appropriate node for TokenCat.
mkBuildTokenFunction :: Cat -> Doc
mkBuildTokenFunction tokenCat = vcat
    [ text $ "func" +++ fnName ++ "(_ ctx: Token) throws ->" +++ returnType +++ "{"
    , indent 2 $ "return" +++ returnType ++ "(" ++ value ++ ")"
    , "}"
    ]
  where
    tokenName = catToStr tokenCat
    fnName = mkBuildFnName tokenCat
    returnType = catToSwiftType tokenCat
    value = case tokenName of
      "Integer" -> "Int(ctx.getText()!)!"
      "Double"  -> "Double(ctx.getText()!)!"
      _         -> "ctx.getText()!"

-- | generate name for function which will build node for some cat.
mkBuildFnName :: Cat -> String
mkBuildFnName cat = "build" ++ firstUpperCase (restName cat)
  where
    restName cat = case cat of
      ListCat cat  -> restName cat ++ "List"
      TokenCat cat -> cat ++ "Token"
      otherCat     -> catToStr otherCat

mkBuildFunction :: String -> RuleData -> Doc
mkBuildFunction lang (cat, rulesWithLabels)  = vcat
    [ text $ "func" +++ mkBuildFnName cat ++ "(_ ctx: " ++ (addParserPrefix lang $ identCat cat) ++ "Context) throws ->" +++ catToSwiftType cat +++ "{"
    , indent 2 "switch ctx {"
    , vcat $ map mkCaseStmt datas
    , indent 4 "default:"
    , indent 6 $ mkThrowErrorStmt cat
    , indent 2 "}"
    , "}"
    ]
  where
    datas = zip rulesWithLabels [1..]

    mkCaseStmt :: ((String, SentForm), Integer) -> Doc
    mkCaseStmt ((ruleLabel, rhsRule), ifIdx) = vcat
        [ indent 4 $ "case let ctx as" +++ addParserPrefix lang (antlrRuleLabel cat ruleLabel antlrRuleLabelIdx) ++ "Context:"
        , vcat $ map text $ mCaseBody ruleLabel
        ]

      where
        antlrRuleLabelIdx = if isCoercion ruleLabel then Just ifIdx else Nothing
        rhsRuleWithIdx = mapMaybe (\(rule, idx) -> either (\cat -> Just (cat, idx)) (\_ -> Nothing) rule) $ zip rhsRule [1..]
        mkPattern idx = "p_" ++ show ifIdx ++ "_" ++ show idx
        -- mkPattern idx = "expr(" ++ show idx ++ ")!"

        mCaseBody ruleLabel
          | isCoercion ruleLabel = map (\(cat, idx) -> indentStr 6 $ "return try" +++ mkBuildFnName cat ++ "(ctx." ++ mkPattern idx ++ ")") rhsRuleWithIdx
          | isNilFun ruleLabel   = emptyListBody
          | isOneFun ruleLabel   = oneListBody
          | isConsFun ruleLabel  = consListBody
          | otherwise            =
              concat
                [ zipWith
              (\ (cat, idx) varName
                  -> indentStr 6
                      $ "let" +++ varName
                          +++ "= try" +++ mkBuildFnName cat ++ "(ctx." ++ mkPattern idx ++ ")")
                            rhsRuleWithIdx varNames
                , [ indentStr 6 returnStatement]
                ]
            where
              varNames = getVarsFromCats rhsCats
              rhsCats = map fst rhsRuleWithIdx
              returnStatementBase = "return" +++ "." ++ ruleLabel
              returnStatement
                | null varNames = returnStatementBase 
                | otherwise     = returnStatementBase ++ "(" ++ intercalate ", " varNames ++ ")"

        emptyListBody = [indentStr 6 "return []"]
        oneListBody = map (\(cat, idx) -> indentStr 6 $ "let data = try" +++ mkBuildFnName cat ++ "(ctx." ++ mkPattern idx ++ ")") rhsRuleWithIdx ++ [ indentStr 4 "return [data]"]
        consListBody =
            [ indentStr 6 $ "let value1 = try" +++  mkBuildFnName firstCat ++ "(ctx." ++ mkPattern firstIdx ++ ")"
            , indentStr 6 $ "let value2 = try" +++  mkBuildFnName secondCat ++ "(ctx." ++ mkPattern secondIdx ++ ")"
            , indentStr 6 $ "return" +++ resultList
            ]
          where
            (firstCat, firstIdx) = head rhsRuleWithIdx
            (secondCat, secondIdx) = rhsRuleWithIdx !! 1
            (itemVar, listVar) = if isList firstCat then ("value2", "value1") else ("value1", "value2")
            resultList = if isList firstCat
              then
                listVar +++ "+" +++ "[" ++ itemVar ++ "]"
              else
                "[" ++ itemVar  ++ "]" +++ "+" +++ listVar

cfToGroups :: CF -> [RuleData]
cfToGroups cf = map (second (map (ruleToData . makeLeftRecRule cf))) $ ruleGroups cf
  where
    ruleToData rule = ((wpThing . funRule) rule, rhsRule rule)


addParserPrefix :: String -> String -> String
addParserPrefix lang name = lang ++ "Parser." ++ name