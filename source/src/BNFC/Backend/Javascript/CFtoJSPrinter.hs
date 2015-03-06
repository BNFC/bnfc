module BNFC.Backend.Javascript.CFtoJSPrinter (jSPrinter) where

import Data.List
import Data.Either
import Text.Printf

import BNFC.CF
import BNFC.Backend.Common.NamedVariables

jSPrinter :: CF -> SymEnv -> String
jSPrinter cf _ = unlines [
    "var Visitor = exports.Visitor = function () {"
    , "\tthis.text = '';"
    , "\tthis.indent = 0;"
    , "};\n"
    , "Visitor.prototype.visit = function (node) {"
    , "\treturn this[node.type](node);"
    , "};\n"
    , "Visitor.prototype.addIndent = function () {"
    , "\tthis.text += Array(this.indent + 1).join('\\t');"
    , "};\n"
    , "Visitor.prototype.removeIndent = function () {"
    , "\tif (this.text.substr(-1) == '\\t') this.text = this.text.slice(0, -1);"
    , "};\n"
    , "Visitor.prototype.visitPrec = function (node1, node2) {"
    , printf "\tvar levels = { %s };" $ jSLevels $ jSNames cf
    , "\tif (levels[node1.type] > levels[node2.type]) this.text += '(';"
    , "\tthis.visit(node2);"
    , "\tif (levels[node1.type] > levels[node2.type]) {"
    , "\t\tif (this.text.substr(-1) == ' ') this.text = this.text.slice(0, -1);"
    , "\t\tthis.text += ')';"
    , "\t}"
    , "};\n"
    , intercalate "\n\n" $ jSMethods $ jSNames cf
  ]


jSLevels :: [Either String Rule] -> String
jSLevels = intercalate ", " . (map level) . rights
    where level r = printf "\"%s\": %d" (funRule r) (precRule r)

jSNames :: CF -> [Either String Rule]
jSNames cf = rules ++ tokens
    where rules  = map Right $ filter remove $ concatMap snd $ ruleGroups cf
          tokens = map (Left . show . fst) $ tokenPragmas cf
          remove r = let x = funRule r in not $ isCoercion x

jSMethods :: [Either String Rule] -> [String]
jSMethods rs = map method rs
    where
        method r = printf "Visitor.prototype.%s = function (node) {\n%s\n};"
                    (name r) (jSAction r)
        name = either id ruleName

ruleName :: Rule -> String
ruleName r
    | isNilFun (funRule r) = (cat r) ++ "Nil"
    | isOneFun (funRule r) = (cat r) ++ "One"
    | isConsFun (funRule r) = (cat r) ++ "Cons"
    | otherwise = funRule r
    where cat = identCat . valCat

jSAction :: Either String Rule -> String
jSAction (Left _) = "\tthis.text += node.args[0] + ' ';" -- Tokens
jSAction (Right r) -- Rules
    | isList (valCat r) = intercalate "\n" $ map f ts
    | otherwise = intercalate "\n" $ map f cs
    where cs = jSArgs 0 $ rhsRule r
          ts = map (\l -> (0, Right l)) $ rights $ rhsRule r
          f = uncurry (jSRhs $ precCat (valCat r))

-- Arguments numbers, skip tokens as they are not in the argument array
jSArgs :: Integer -> [Either Cat String] -> [(Integer, Either Cat String)]
jSArgs _ [] = []
jSArgs i (Left c:xs)  = (i, Left c):jSArgs (i+1) xs
jSArgs i (Right s:xs) = (0, Right s):jSArgs i xs

jSRhs :: Integer -> Integer -> Either Cat String -> String
jSRhs _ 0 (Right s) -- User tokens
    | s `elem` norspace = printf "\tthis.text += '%s';" (escape s)
    | s `elem` nolspace = pop ++ (printf "\tthis.text += '%s '" (escape s))
    | s == ";" = "\tthis.text += ';\\n'; this.addIndent();"
    | s == "{" = "\tthis.text += '{\\n'; this.indent++; this.addIndent();"
    | s == "}" = "\tthis.indent--; this.removeIndent(); this.text += '}\\n'; this.addIndent();"
    | otherwise = printf "\tthis.text += '%s ';" (escape s)
    where norspace = ["[", "("]
          nolspace = ["]", ")", ","]
          pop = unlines $ [
            "\tif (this.text.substr(-1) === ' ')"
            , "\t\tthis.text = this.text.slice(0, -1);"
            ]
jSRhs _ i (Left c) -- Cats
    | isList c = printf listStr i i cat i cat cat i cat -- so many arguments ...
    | (show c) `elem` specialCatsP = case (show c) of
                                    "String" -> printf "\tthis.text += '\"' + node.args[%d] + '\" '" i
                                    "Char" -> printf "\tthis.text += '\\'' + node.args[%d] + '\\' '" i
                                    _ -> printf "\tthis.text += node.args[%d] + ' '" i
    | otherwise = printf "\tthis.visitPrec(node, node.args[%d]);" i
    where listStr = unlines $ [
            "\tfor (var i=0; i < node.args[%d].length; i++) {"
            , "\t\t" ++ action
            , "\t\tif (this.%sCons && i+1 < node.args[%d].length) this.%sCons();"
            , "\t\tif (!this.%sOne && i+1 == node.args[%d].length) this.%sCons();"
            , "\t}"
            ]
          action = if (show $ catOfList c) `elem` specialCatsP
                    then "\tthis.text += node.args[%d][i] + ' ';"
                    else "\tthis.visitPrec(node, node.args[%d][i]);"
          cat = identCat c
jSRhs _ _ _ = error "unknown rhs"

-- | Helpers
escape s = concatMap (\c -> if c `elem` chars then '\\':[c] else [c]) s
    where chars = "'"
