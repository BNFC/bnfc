module BNFC.Backend.Java.Utils where
import BNFC.Backend.Common.NamedVariables
import BNFC.Utils ( mkName, NameStyle(..))

javaReserved = [
    "abstract"   ,"continue"   ,"for"   ,"new"   ,"switch"
    ,"assert"  ,"default"   ,"goto"   ,"package"   ,"synchronized"
    ,"boolean"   ,"do"   ,"if"   ,"private"   ,"this"
    ,"break"   ,"double"   ,"implements"   ,"protected"   ,"throw"
    ,"byte"   ,"else"   ,"import"   ,"public"   ,"throws"
    ,"case"   ,"enum"   ,"instanceof"   ,"return"   ,"transient"
    ,"catch"   ,"extends"   ,"int"   ,"short"   ,"try"
    ,"char"   ,"final"   ,"interface"   ,"static"   ,"void"
    ,"class"   ,"finally"   ,"long"   ,"strictfp"   ,"volatile"
    ,"const"   ,"float"   ,"native"   ,"super"   ,"while"
    ]

getRuleName z = if x `elem` ("grammar" : javaReserved) then z ++ "_" else z
                where x = firstLowerCase z

getLabelName = mkName ["Rule"] CamelCase

getLastInPackage :: String -> String
getLastInPackage =
    last . words . map (\c -> if c == '.' then ' ' else c)
