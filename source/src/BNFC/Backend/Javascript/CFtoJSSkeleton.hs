module BNFC.Backend.Javascript.CFtoJSSkeleton (jSSkeleton) where

import Data.List
import Data.Either
import Text.Printf

import BNFC.CF
import BNFC.Backend.Common.NamedVariables

jSSkeleton :: CF -> SymEnv -> String
jSSkeleton cf _ = unlines [
    "var Visitor = exports.Visitor = function () {"
    , "};\n"
    , "Visitor.prototype.visit = function (node) {"
    , "\treturn this[node.type](node);"
    , "};\n"
    , intercalate "\n\n" $ jSMethods $ jSNames cf
  ]

jSNames :: CF -> [Either String Rule]
jSNames cf = rules ++ tokens
    where rules  = map Right $ filter remove $ concatMap snd $ ruleGroups cf
          tokens = map (Left . show . fst) $ tokenPragmas cf
          remove r = let x = funRule r
                      in not $ isCoercion x || isConsFun x
                               || isOneFun x || isNilFun x

jSMethods :: [Either String Rule] -> [String]
jSMethods rs = map method rs
    where method r = printf "Visitor.prototype.%s = function (node) {\n%s\n};"
                      (name r) (jSAction r)
          name = either id funRule

jSAction :: Either String Rule -> String
jSAction (Left _) = "return new Error(\"failure\");"
jSAction (Right r) = printf "\t// args: %s\n\treturn new Error(\"failure\")"
                      (intercalate ", " $ map show $ lefts $ rhsRule r)
