module BNFC.Backend.Python.AntlrAdapter(generateAntlrAction, 
                                pyAntlrMembers, 
                                pyAntlrHeader) where


import BNFC.Backend.Common.MultipleParserGenerationTools (ToolParameters (..))
import BNFC.CF
import BNFC.Backend.Python.Utils
import BNFC.Backend.Python.AbsPython
import BNFC.Backend.Common.NamedVariables
import BNFC.Utils ( (+++), (+.+))
import Data.List
import Text.PrettyPrint

-- Type declarations
type Rules       = [(NonTerminal,[(Pattern, Fun, Action)])]
type Pattern     = String
type Action      = String
type MetaVar     = (String, Cat)

result :: Entity
result = mkId "$result"

assignResult :: Entity -> Entity
assignResult e = Assignment [result] [e]

assignNewAbsynObject :: String -> [Entity] -> Entity
assignNewAbsynObject s args = assignResult $ Function (mkId s) args 

generateAntlrAction :: ToolParameters -> NonTerminal -> Fun -> [MetaVar]
               -> Bool   -- ^ Whether the list should be reversed or not.
                         --   Only used if this is a list rule.
               -> Action
generateAntlrAction tpar nt f ms rev  
    | isNilFun f = show $ absVcat [assignNewAbsynObject c []]
    | isOneFun f = "$result = new " ++ c ++ "(); $result.addLast("
        ++ p_1 ++ ");"
    | isConsFun f = "$result = " ++ p_2 ++ "; "
                           ++ "$result." ++ add ++ "(" ++ p_1 ++ ");"
    | isCoercion f = "$result = " ++  p_1 ++ ";"
    | isDefinedRule f = "$result = parser." ++ f ++ "_"
                        ++ "(" ++ intercalate "," (map resultvalue ms) ++ ");"
    | otherwise = "$result = new " ++ c
                  ++ "(" ++ posInfo ++ intercalate "," (map resultvalue ms) ++ ");"
   where
     positionString    = "_ctx.getStart().getLine(), _ctx.getStart().getCharPositionInLine()"
     posInfo           = if (preservePositions tpar)
                            then if ms == [] then positionString else positionString++","
                            else ""
     c                 =  if isNilFun f || isOneFun f || isConsFun f
                            then identCat (normCat nt) else f
     p_1               = resultvalue $ ms!!0
     p_2               = resultvalue $ ms!!1
     add               = if rev then "addLast" else "addFirst"
     gettext           = "getText()"
     removeQuotes x    = "substring(1, "++ x +.+ gettext +.+ "length()-1)"
     parseint x        = "Integer.parseInt("++x++")"
     parsedouble x     = "Double.parseDouble("++x++")"
     charat            = "charAt(1)"
     resultvalue (n,c) = case c of
                          TokenCat "Ident"   -> n'+.+gettext
                          TokenCat "Integer" -> parseint $ n'+.+gettext
                          TokenCat "Char"    -> n'+.+gettext+.+charat
                          TokenCat "Double"  -> parsedouble $ n'+.+gettext
                          TokenCat "String"  -> n'+.+gettext+.+removeQuotes n'
                          _         -> (+.+) n' (if isTokenCat c then gettext else "result")
                          where n' = '$':n
                          
generateParserMembers, generateParserHeader :: [Entity]
generateParserMembers = [
                        classMethodDefinition (mkId "__prepend") [l, e] [
                            Assignment [l] [Plus listE l]
                            , Return l
                            ]
                        , classMethodDefinition (mkId "__append") [l, e] [
                            Function (toNames [l, mkId "append"]) [e]
                            , Return l
                          ]
                        ]
                        where
                            l = mkId "l"
                            e = mkId "e"
                            listE = listSingleton e
                            
generateParserHeader =  [
                       From $ Ident "Absyn"                        
                       ]
                       
pyAntlrHeader, pyAntlrMembers :: String
pyAntlrHeader = render $ absVcat generateParserHeader
pyAntlrMembers = render $ absVcat generateParserMembers
