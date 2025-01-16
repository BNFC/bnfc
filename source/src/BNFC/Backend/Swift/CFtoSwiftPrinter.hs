module BNFC.Backend.Swift.CFtoSwiftPrinter (cf2SwiftPrinter) where

import Data.Either (lefts, rights)
import Data.List (nub, intercalate, find, uncons, intersperse)

import Text.PrettyPrint.HughesPJClass (Doc, text, vcat, hcat, nest)

import BNFC.CF (CF, ruleGroups, Rul (rhsRule, funRule), Cat (Cat, ListCat, TokenCat, CoercCat), WithPosition (wpThing), IsFun (isCoercion, isConsFun, isOneFun, isNilFun), catToStr, SentForm, rulesForNormalizedCat, normCat, normCatOfList, catOfList, isList, allParserCats, rulesForCat)
import BNFC.Utils ((+++))
import BNFC.Backend.Swift.Common (catToSwiftType, indent, wrapSQ, getVarsFromCats, getAbsynWithoutLists, getAllTokenTypenames, getAllTokenCats, mkTypeName, wrapIfNeeded)
import BNFC.Backend.Common.NamedVariables (firstUpperCase)
import Data.Maybe (isJust, isNothing, fromMaybe)

prettyPrintProtocolName :: String
prettyPrintProtocolName = "PrettyPrintable"

prettyPrintPropertyName :: String
prettyPrintPropertyName = "printed"

prettyPrintRenderClassName :: String
prettyPrintRenderClassName = "Rendered"

prettyPrintRenderCall :: String
prettyPrintRenderCall = prettyPrintRenderClassName ++ ".shared.render"

-- | generate pretty-printers for nodes of an AST
cf2SwiftPrinter :: CF -> Doc
cf2SwiftPrinter cf = vcat
    [ protocolDeclaration
    , rendererDeclaration
    , tokenPrinterDecl
    , nodesPrintersDecls
    , ""
    , nodesPrettifiersDecls
    ]
  where
    tokenPrinterDecl = mkTokenPrinter cf

    -- we intentionally want to have rules for list cats, which have items of type Coerc
    cats = let isCoercCat (CoercCat _ _) = True
               isCoercCat _              = False
            in filter (not . isCoercCat) $ allParserCats cf

    nodesPrettifiersDecls = vcat $ intersperse (text "") $ map (mkNodePrettifier cf) cats
    nodesPrintersDecls = vcat $ intersperse (text "") $ (map mkNodePrinter cats)

    rules = map (wpThing . funRule) $
              concatMap
              (filter (not . isCoercion) . rulesForNormalizedCat cf)
              (filter (not . isList) cats)

protocolDeclaration :: Doc
protocolDeclaration = vcat
  [ text $ "public protocol" +++ prettyPrintProtocolName +++ "{"
  , nest 2 $ text $ "var" +++ prettyPrintPropertyName +++ ": String { get }"
  , "}"
  , ""
  ]

rendererDeclaration :: Doc
rendererDeclaration = vcat
    [ text $ "final class" +++ className +++ "{"
    , nest 2 $ tokenEnumDeclaration
    , nest 2 $ text $ "static let shared =" +++ className ++ "()"
    , nest 2 $ text $ "private let indentSize = 2"
    , text ""
    , nest 2 "private init() {}"
    , text ""
    , nest 2 renderFunctionDeclaration
    , nest 2 transformFunctionDeclaration
    , nest 2 groupTokensFunctionDeclaration
    , nest 2 addIndentationFunctionDeclaration
    , nest 2 dropTrailingSpacesFunctionDeclaration
    , nest 2 dropTrailingNewlinesFunctionDeclaration
    , "}"
    , ""
    ]
  where
    className = prettyPrintRenderClassName
    tokenEnumDeclaration = vcat
        [ "private enum Token {"
        , nest 2 body
        , "}"
        , ""
        ]
      where 
        body = vcat 
          [ "case text(value: String)"
          , "case newline(indentShift: Int)"
          , "case space"
          , ""
          , "func toString() -> String {"
          , indent 2 "switch self {"
          , indent 4 "case .text(let value):"
          , indent 6 "return value"
          , indent 4 "case .newline:"
          , indent 6 "return \"\\n\""
          , indent 4 "case .space:"
          , indent 6 "return \" \""
          , indent 2 "}"
          , "}"
          ]

    renderFunctionDeclaration = vcat 
        [ "func render(_ tokens: [String]) -> String {"
        , nest 2 body
        , "}"
        , ""
        ]
      where
        body = vcat
          [ "let transformedTokens = transform(tokens)"
          , "let groupedTokens = groupTokens(transformedTokens)"
          , "return groupedTokens"
          , indent 2 ".map { addIndentation(to: $0) }"
          , indent 2 ".map { $0.map { $0.toString() }.joined() }"
          , indent 2 ".joined(separator: \"\\n\")"
          ]

    transformFunctionDeclaration = vcat
        [ "private func transform(_ tokens: [String]) -> [Token] {"
        , nest 2 body
        , "}"
        , ""
        ]
      where
        body = vcat 
          [ "var result: [Token] = []"
          , "for token in tokens {"
          , indent 2 "switch token {"
          , nest 4 casesDeclaration
          , indent 2 "}"
          , "}"
          , "dropTrailingSpaces(from: &result)"
          , "dropTrailingNewlines(from: &result)"
          , "return result"
          ]
        casesDeclaration = vcat
          [ "case \"\", \" \":"
          , indent 2  "continue"
          , "case \"{\":"
          , indent 2 "result.append(.text(value: token))"
          , indent 2 "result.append(.newline(indentShift: 1))"
          , "case \"}\":"
          , indent 2 "dropTrailingNewlines(from: &result)"
          , indent 2 "result.append(.newline(indentShift: -1))"
          , indent 2 "result.append(.text(value: token))"
          , indent 2 "result.append(.newline(indentShift: 0))"
          , "case \"(\", \")\", \"[\", \"]\", \"<\", \">\", \",\", \".\":"
          , indent 2 "dropTrailingSpaces(from: &result)"
          , indent 2 "if token == \")\" || token == \"]\" || token == \"}\" {"
          ,  indent 4 "dropTrailingNewlines(from: &result)"
          , indent 2 "}"
          , indent 2 "result.append(.text(value: token))"
          , indent 2 "if token != \".\" {"
          , indent 4 "result.append(.space)"
          , indent 2 "}"
          , "case \";\":"
          , indent 2 "dropTrailingSpaces(from: &result)"
          , indent 2 "dropTrailingNewlines(from: &result)"
          , indent 2 "result.append(.text(value: token))"
          , indent 2 "result.append(.newline(indentShift: 0))"
          , "default:"
          , indent 2 "result.append(.text(value: token))"
          , indent 2 "result.append(.space)" 
          ]
    
    groupTokensFunctionDeclaration = vcat 
        [ "private func groupTokens(_ tokens: [Token]) -> [(indentationLevel: Int, tokens: [Token])] {"
        , indent 2 "var groups: [(indentationLevel: Int, tokens: [Token])] = []"
        , indent 2 "var currentIndentation = 0"
        , indent 2 "for token in tokens {"
        , indent 4 "if case .newline(let shift) = token {"
        , indent 6 "currentIndentation += shift"
        , indent 6 "groups.append((currentIndentation, []))"
        , indent 4 "} else {"
        , indent 6 "if groups.isEmpty {"
        , indent 8 "groups.append((currentIndentation, []))"
        , indent 6 "}"
        , indent 6 "groups[groups.count - 1].tokens.append(token)"
        , indent 4 "}"
        , indent 2 "}"
        , indent 2 "return groups"
        , "}"
        , ""
        ]
    
    addIndentationFunctionDeclaration = vcat 
        [ "private func addIndentation(to group: (indentationLevel: Int, tokens: [Token])) -> [Token] {"
        , indent 2 "var tokens = group.tokens"
        , indent 2 "if group.indentationLevel > 0 {"
        , indent 4 "tokens.insert(.text(value: String(repeating: \" \", count: indentSize * group.indentationLevel)), at: 0)"
        , indent 2 "}"
        , indent 2 "return tokens"
        , "}"
        , ""
        ]
    
    dropTrailingSpacesFunctionDeclaration = vcat
        [ "private func dropTrailingSpaces(from tokens: inout [Token]) {"
        , indent 2 "while let last = tokens.last, case .space = last {"
        , indent 4 "tokens.removeLast()"
        , indent 2 "}"
        , "}"
        , ""
        ]
    
    dropTrailingNewlinesFunctionDeclaration = vcat 
        [ "private func dropTrailingNewlines(from tokens: inout [Token]) {"
        , indent 2 "while let last = tokens.last, case .newline = last {"
        , indent 4 "tokens.removeLast()"
        , indent 2 "}"
        , "}"
        ]

-- | generate function which will print user-defined and predefined tokens.
mkTokenPrinter :: CF -> Doc
mkTokenPrinter cf = vcat
    [ tokenPrinters ]
  where
    -- allTokenTypes = getAllTokenTypenames cf
    -- tokensUnionType = intercalate " | " allTokenTypes

    tokenPrinters = vcat $ map mkTokenPrinter (getAllTokenCats cf)
    mkTokenPrinter tokenCat = vcat
      [ text $ "extension" +++ catToSwiftType tokenCat ++ ":" +++ prettyPrintProtocolName ++ "{" 
      , nest 2 $ text $ "public var" +++ prettyPrintPropertyName ++ ": String {"
      , indent 4 "String(value)"
      , indent 2 "}"
      , "}"
      , ""
      ]

mkNodePrinter :: Cat -> Doc
mkNodePrinter cat@(Cat _) = vcat
    [ text $ "extension" +++ catToSwiftType cat ++ ":" +++ prettyPrintProtocolName ++ "{"
    , nest 2 $ text $ "public var" +++ prettyPrintPropertyName ++ ": String {"
    , indent 4 $ prettyPrintRenderCall ++ "(" ++ prettifyFnName ++ "(self))"
    , indent 2 "}"
    , "}"
    ]
  where
    printFnName    = mkPrintFnName cat
    prettifyFnName = mkPrettifyFnName cat

mkNodePrinter listCat@(ListCat _) = vcat
    [ text $ "extension" +++ catToSwiftType listCat ++ ":" +++ prettyPrintProtocolName ++ "{"
    , nest 2 $ text $ "public var" +++ prettyPrintPropertyName ++ ": String {"
    , indent 4 $ prettyPrintRenderCall ++ "(" ++ prettifyFnName ++ "(self))"
    , indent 2 "}"
    , "}"
    ]
  where
    prettifyFnName = mkPrettifyFnName listCat
    printFnName    = mkPrintFnName listCat
    catOfListType  = catToSwiftType (normCatOfList listCat)

mkNodePrinter otherCat = error $ "Unknown category for making node printer" +++ catToStr otherCat

mkRulePrinter :: String -> Doc
mkRulePrinter ruleLabel = vcat
    [ text $ "func" +++ printFnName ++ "(node:" +++ mkTypeName ruleLabel ++ ") -> String {"
    , indent 2 $ "return" +++ prettyPrintRenderCall ++ "(" ++ prettifyFnName ++ "(node))"
    , "}"
    ]
  where
    printFnName    = "print" ++ firstUpperCase ruleLabel
    prettifyFnName = "prettify" ++ firstUpperCase ruleLabel

mkNodePrettifier :: CF -> Cat -> Doc
mkNodePrettifier cf cat@(Cat _) = vcat $ concat
    [ [text $ "func" +++ prettifyFnName ++ "(_ node:" +++ catToSwiftType cat ++ ") -> [String] {" ]
    , [indent 2 $ "switch node {"]
    , prettifyRulesCondition
    , [indent 2 "}"]
    , ["}"]
    -- , rulesPrettifiers
    ]
  where      
    rules = map (\rule -> (wpThing (funRule rule), rhsRule rule)) $
              filter (not . isCoercion . funRule) $
              rulesForNormalizedCat cf cat
    
    mkCaseStmt rule@(ruleLabel, sentForm) = vcat
        [ indent 4 $ caseDeclaration ++ ruleLabel ++ (associatedValues varNames) ++ ":"
        , nest 6 $ hcat [ text "return ", (mkRulePrettifier rule)]
        ]
      where
        varNames = map wrapIfNeeded $ getVarsFromCats (lefts sentForm)

        caseDeclaration
          | null varNames = "case ."
          | otherwise     = "case let ."

        associatedValues varNames
          | null varNames = ""
          | otherwise     = "(" ++ (intercalate ", " varNames) ++ ")"


    prettifyRulesCondition = map mkCaseStmt rules
    prettifyFnName = mkPrettifyFnName cat
    -- rulesPrettifiers = map mkRulePrettifier rules
    -- add getVarsFromCats

mkNodePrettifier cf listCat@(ListCat _) = vcat
    [ text $ "func " ++ prettifyFnName ++ "(_ list: [" ++ catOfListType ++ "]) -> [String] {"
    , nest 2 returnStmt
    , "}"
    ]
  where
    prettifyFnName = mkPrettifyFnName listCat
    catOfListType = catToSwiftType (normCatOfList listCat)

    rules = rulesForCat cf listCat
    consRule = find (isConsFun . funRule) rules
    consSeparator = maybe Nothing findSeparator consRule

    oneRule = find (isOneFun . funRule) rules
    oneSeparator = maybe Nothing findSeparator oneRule

    nilRule = find (isNilFun . funRule) rules

    findSeparator :: Rul a -> Maybe String
    findSeparator rule = fmap fst (uncons terminals)
      where
        terminals = rights (rhsRule rule)

    separator = fromMaybe "" consSeparator
    isTerminator = (isJust nilRule && isNothing oneRule && isJust consRule && isJust consSeparator)
      || (isNothing nilRule && isJust oneRule && isJust oneSeparator && isJust consRule && isJust consSeparator)

    itemCat = catOfList listCat
    printItemFn tokenCat@(TokenCat _) = mkPrintFnName tokenCat
    printItemFn cat                  = mkPrettifyFnName cat

    returnStmt = text listTokens
      where
        listMapping = "list.flatMap { item in " ++ printItemFn itemCat ++ "(item) + " ++ "[" ++ wrapSQ separator ++ "] }"
        listTokens = listMapping
        --   listMapping ++ if isTerminator then "" else ".dropLast()" -- TODO: check

mkNodePrettifier _ otherCat = error $ "Unknown category for making node prettifier" +++ catToStr otherCat

mkRulePrettifier :: (String, SentForm) -> Doc
mkRulePrettifier (ruleLabel, sentForm) = vcat
    [ text prettifyBody ]
  where
    varNames = map wrapIfNeeded $ getVarsFromCats (lefts sentForm)

    addVarNames :: [Either Cat String] -> [String] -> [Either (Cat, String) String]
    addVarNames [] _                      = []
    addVarNames list []                   = map (either (\cat -> Left (cat, "")) Right) list
    addVarNames (x:xs) allVars@(var:vars) = case x of
      (Right terminal)  -> Right terminal : addVarNames xs allVars
      (Left cat)        -> Left (cat, var) : addVarNames xs vars
    
    sentFormWithVarNames = addVarNames sentForm varNames
    prettifiedRule = intercalate " + " $ map (either getPrettifierForCat (\x -> "[" ++ (wrapSQ x) ++ "]")) sentFormWithVarNames
      where
        getPrettifierForCat :: (Cat, String) -> String
        getPrettifierForCat (tokenCat@(TokenCat _), varName) = "[" ++ varName ++ "." ++ prettyPrintPropertyName ++ "]"
        getPrettifierForCat (cat, varName)                   = mkPrettifyFnName cat ++ "(" ++ varName ++ ")"

    prettifyBody
      | null sentFormWithVarNames = "[]"
      | otherwise                 = prettifiedRule

mkPrettifyFnName :: Cat -> String
mkPrettifyFnName cat = "prettify" ++ mkName cat
  where
    mkName (ListCat cat) = ("ListOf"++) $ firstUpperCase (catToStr cat)
    mkName otherCat      = firstUpperCase $ catToStr (normCat otherCat)

mkPrintFnName :: Cat -> String
mkPrintFnName cat = "print" ++ mkName cat
  where
    mkName (ListCat itemCat) = "ListOf" ++ firstUpperCase (catToStr itemCat)
    mkName otherCat          = firstUpperCase $ catToStr (normCat otherCat)