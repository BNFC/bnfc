{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartPrinter (cf2DartPrinter) where

import BNFC.CF
import BNFC.Backend.Dart.Common
import BNFC.Utils       ( (+++) )
import Data.Maybe      ( mapMaybe )
import Data.List ( intercalate, find )
import Data.Either ( isLeft )

cf2DartPrinter :: CF -> String
cf2DartPrinter cf = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in
    unlines $
      imports ++
      helperFunctions ++
      stringRenderer ++
      (map buildUserToken userTokens) ++
      (concatMap generateRulePrinters $ getAbstractSyntax cf) ++
      (concatMap generateLabelPrinters $ ruleGroups cf)

imports :: [String]
imports = [
  "import 'ast.dart' as ast;",
  "import 'package:fast_immutable_collections/fast_immutable_collections.dart';" ]

helperFunctions :: [String]
helperFunctions = [
  "sealed class Token {}",
  "",
  "class Space extends Token {}",
  "",
  "class NewLine extends Token {",
  "  int indentDifference;",
  "  NewLine.indent(this.indentDifference);",
  "  NewLine() : indentDifference = 0;",
  "  NewLine.nest() : indentDifference = 1;",
  "  NewLine.unnest() : indentDifference = -1;",
  "}",
  "",
  "class Text extends Token {",
  "  String text;",
  "  Text(this.text);",
  "}" ]

stringRenderer :: [String]
stringRenderer = [
  "class StringRenderer {",
  "  // Change this value if you want to change the indentation length",
  "  static const _indentInSpaces = 2;",
  "",
  "  String print(IList<String> tokens) => tokens",
  "      .fold(IList<Token>(), _render)",
  "      .fold(IList<(int, IList<Token>)>(), _split)",
  "      .map((line) => (line.$1, line.$2.map(_tokenToString).join()))",
  "      .fold(IList<(int, String)>(), _convertIndentation)",
  "      .map(_addIndentation)",
  "      .join();",
  "",
  "  IList<(int, IList<Token>)> _split(",
  "    IList<(int, IList<Token>)> lists,",
  "    Token token,",
  "  ) =>",
  "      switch (token) {",
  "        NewLine nl => lists.add(",
  "            (",
  "              nl.indentDifference,",
  "              IList([]),",
  "            ),",
  "          ),",
  "        _ => lists.put(",
  "            lists.length - 1,",
  "            (",
  "              lists.last.$1,",
  "              lists.last.$2.add(token),",
  "            ),",
  "          )",
  "      };",
  "",
  "  String _tokenToString(Token t) => switch (t) {",
  "        Text t => t.text,",
  "        Space _ => ' ',",
  "        _ => '',",
  "      };",
  "",
  "  IList<(int, String)> _convertIndentation(",
  "    IList<(int, String)> lines,",
  "    (int, String) line,",
  "  ) =>",
  "      lines.add(",
  "        (",
  "          line.$1 + (lines.lastOrNull?.$1 ?? 0),",
  "          line.$2,",
  "        ),",
  "      );",
  "",
  "  String _addIndentation((int, String) indentedLine) =>",
  "      ' ' * (_indentInSpaces * indentedLine.$1) + indentedLine.$2;",
  "",
  "  // This function is supposed to be edited",
  "  // in order to adjust the pretty printer behavior",
  "  IList<Token> _render(IList<Token> tokens, String token) => switch (token) {",
  "        '{' => tokens.addAll([Text(token), NewLine.nest()]),",
  "        '}' => tokens.addAll([NewLine.unnest(), Text(token)]),",
  "        ';' => tokens.addAll([NewLine(), Text(token)]),",
  "        ',' ||",
  "        '.' ||",
  "        ':' ||",
  "        '<' ||",
  "        '>' ||",
  "        '[' ||",
  "        ']' ||",
  "        '(' ||",
  "        ')' =>",
  "          tokens.removeTrailingSpaces.addAll([Text(token), Space()]),",
  "        '\\$' || '&' || '@' || '!' || '#' => tokens.add(Text(token)),",
  "        _ => tokens.addAll([Text(token), Space()])",
  "      };",
  "}",
  "",
  "extension TokensList on IList<Token> {",
  "  IList<Token> get removeTrailingSpaces =>",
  "      isNotEmpty && last is Space ? removeLast().removeTrailingSpaces : this;",
  "}",
  "",
  "extension PrintableInt on int {",
  "  String get print => toString();",
  "}",
  "",
  "extension PrintableDouble on double {",
  "  String get print => toString();",
  "}",
  "",
  "final _renderer = StringRenderer();",
  "",
  "mixin Printable {",
  "  String get print => \'[not implemented]\';",
  "}" ]

buildUserToken :: String -> String
buildUserToken token = "String print" ++ token ++ "(x) => x.value;"

generateLabelPrinters :: (Cat, [Rule]) -> [String]
generateLabelPrinters (cat, rawRules) = 
  let rules = [ (wpThing $ funRule rule, rhsRule rule) | rule <- rawRules ]
  in if isList cat 
    then 
      let 
        sep = findSep rules
        term = findTerm rules
        vType = cat2DartType $ normCat cat
      in if sep == "" && term == ""
        then []
        else [
          generateListPrettifier vType sep term,
          generateListPrintFunction vType ]
    else 
      let funs = [ fst rule | rule <- rules ]
      in mapMaybe (generateConcreteMapping cat) rules ++
        (concatMap generatePrintFunction $ map str2DartClassName $ filter representedInAst funs) 
  where
    representedInAst :: String -> Bool
    representedInAst fun = not (
      isNilFun fun ||
      isOneFun fun ||
      isConsFun fun ||
      isConcatFun fun ||
      isCoercion fun )
    findSep :: [(String, [Either Cat String])] -> String
    findSep [] = ""
    findSep ((name, rhs):rest) 
      | isConsFun name = case [ sep | Right sep <- rhs ] of
        (a:_) -> a
        []    -> findSep rest
      | otherwise = findSep rest
    findTerm :: [(String, [Either Cat String])] -> String
    findTerm [] = ""
    findTerm ((name, rhs):rest) 
      | isOneFun name = case [ sep | Right sep <- rhs ] of
        (a:_) -> a
        []    -> findTerm rest
      | otherwise = findTerm rest

generateRulePrinters :: Data -> [String]
generateRulePrinters (cat, rules) = 
  let funs = map fst rules
      fun = catToStr cat
  in 
    if 
      isList cat || 
      isNilFun fun ||
      isOneFun fun ||
      isConsFun fun ||
      isConcatFun fun ||
      isCoercion fun ||
      fun `elem` funs 
    then 
      [] -- the category is not presented in the AST
    else 
      let className = cat2DartClassName cat
      in  (generateRuntimeMapping className $ map fst rules) ++
          (generatePrintFunction className)

generateRuntimeMapping :: String -> [String] -> [String]
generateRuntimeMapping name ruleNames = [ 
  "IList<String> _prettify" ++ name ++ "(ast." ++ name +++ "a) => switch (a) {" ] ++
  (indent 2 $ map mapRule $ map str2DartClassName ruleNames) ++ 
  (indent 1 [ "};" ]) 
  where
    mapRule name = "ast." ++ name +++ "a => _prettify" ++ name ++ "(a),"

generateConcreteMapping :: Cat -> (String, [Either Cat String]) -> Maybe (String)
generateConcreteMapping cat (label, tokens) 
  | isNilFun label ||
    isOneFun label ||
    isConsFun label ||
    isConcatFun label ||
    isCoercion label = Nothing  -- these are not represented in the AST
  | otherwise = -- a standard rule
    let 
      className = str2DartClassName label
      cats = [ normCat cat | Left cat <- tokens ]
      vars = getVars cats
    in Just . unlines $ [ 
      "IList<String> _prettify" ++ className ++ "(ast." ++ className +++ "a) => IList([" ] ++
      (indent 1 $ generateRuleRHS tokens vars []) ++
      ["]);"]

generateListPrettifier :: DartVarType -> String -> String -> String 
generateListPrettifier vType@(n, name) separator terminator = 
  "IList<String> _prettify" ++ printerListName vType ++ "(" ++ 
  printerListType vType +++ "a) => IList([...a.expand((e" ++ show n ++ 
  ") => [\'" ++ separator ++ "\'," +++ 
  (buildArgument (n - 1, name) ("e" ++ show n)) ++
   "],).skip(1)," +++ "\'" ++ terminator ++ "\',]);"

generateRuleRHS :: [Either Cat String] -> [DartVar] -> [String] -> [String]
generateRuleRHS [] _ lines = lines
generateRuleRHS _ [] lines = lines
generateRuleRHS (token:rTokens) (variable@(vType, _):rVariables) lines = case token of
  Right terminal -> 
    generateRuleRHS rTokens (variable:rVariables) $ lines ++ ["\"" ++ terminal ++ "\","]
  Left _ -> generateRuleRHS rTokens rVariables $ 
    lines ++ [ buildArgument vType ("a." ++ buildVariableName variable) ++ "," ]

buildArgument :: DartVarType -> String -> String
buildArgument (0, _) argument = 
  argument ++ ".print"
buildArgument vType@(n, name) argument = 
  "print" ++ printerListName vType ++ "(" ++ argument ++ ")"

generatePrintFunction :: String -> [String]
generatePrintFunction name = [ 
  "String print" ++ name ++ "(ast." ++ name +++ "x)" +++ "=> _renderer.print(_prettify" ++ name ++ "(x));" ]

generateListPrintFunction :: DartVarType -> String
generateListPrintFunction dvt = 
  "String print" ++ printerListName dvt ++ "(" ++ printerListType dvt +++ "x)" +++ "=> _renderer.print(_prettify" ++ printerListName dvt ++ "(x));" 

printerListName :: DartVarType -> String
printerListName (0, name) = str2DartClassName name
printerListName (n, name) = "List" ++ (printerListName (n - 1, name))

printerListType :: DartVarType -> String
printerListType (0, name) = "ast." ++ (str2DartClassName name)
printerListType (n, name) = "List<" ++ printerListType (n - 1, name) ++ ">"