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

action :: [Entity] -> String
action x = show $ absVcat x

generateAntlrAction :: ToolParameters -> NonTerminal -> Fun -> [MetaVar]
               -> Bool   -- ^ Whether the list should be reversed or not.
                         --   Only used if this is a list rule.
               -> Action
generateAntlrAction tpar nt f ms rev = 
        action $ [assignResult $ generateAntlrActionEntity tpar nt f ms rev]

generateAntlrActionEntity :: ToolParameters -> NonTerminal -> Fun -> [MetaVar]
               -> Bool   -- ^ Whether the list should be reversed or not.
                         --   Only used if this is a list rule.
               -> Entity
generateAntlrActionEntity tpar nt f ms rev  
    | isNilFun f = emptyList
    | isOneFun f = call __append [emptyList, p_1]
    | isConsFun f = call add [p_2 , p_1]
    | isCoercion f = NothingPython --p_1 
    | isDefinedRule f = NothingPython --"parser",  mkId f,  "_" -- no idea of what the fuck this is
                        -- ++ "(" ++ intercalate "," (map resultvalue ms) ++ ")"
    | otherwise = NothingPython --"$result = " ++ c
                  -- ++ "(" ++ posInfo ++ intercalate "," (map resultvalue ms) ++ ")"
   where
--      positionString    = "_localctx.start.line, _localctx.start.start"
--      posInfo           = if (preservePositions tpar)
--                             then if ms == [] then positionString else positionString++","
--                             else ""
     c                 =  if isNilFun f || isOneFun f || isConsFun f
                            then identCat (normCat nt) else f
     p_1               = resultvalue $ ms!!0
     p_2               = resultvalue $ ms!!1
     call what how     = Function (toNames [Self, what]) how
     __append          = mkId "__append"
     __prepend         = mkId "__prepend"
     add               = if rev then __append else __prepend
     gettext           = mkId "text"
     removeQuotes      = YesArray $ mkId "1:-1"
     parseint y        = coercion "int" y
     parsedouble y     = coercion "float" y
     coercion x y      = Function (mkId x) [y]
     charat            = listSingleton $ mkId "1"
     resultvalue (n,c) = case c of
                          TokenCat "Ident"   -> toNames [n', gettext]
                          TokenCat "Integer" -> parseint $ toNames [n', gettext]
                          TokenCat "Char"    -> toNames [n', gettext, charat]
                          TokenCat "Double"  -> parsedouble $ toNames [n', gettext]
                          TokenCat "String"  -> SquareBracketAccess (toNames [n', gettext]) removeQuotes 
                          _         -> toNames [n', (if isTokenCat c then gettext else mkId "result")]
                          where n' = mkId ('$':n)


generateParserMembers, generateParserHeader :: [Entity]
generateParserMembers = __prepend ++ __append
                        where
                            l = mkId "l"
                            e = mkId "e"
                            listE = listSingleton e
                            met na body = [Method $ Function (mkId na) [Self, l ,e]
                                          , IndentedBlock body]
                            __prepend = met "__prepend" [
                                        Assignment [l] [Plus listE l]
                                        , Return l]
                            __append  = met "__append" [
                                        Function (toNames [l, mkId "append"]) [e]
                                        , Return l]
                            
                            
generateParserHeader =  [
                       From $ Ident "Absyn"                        
                       ]
                       
pyAntlrHeader, pyAntlrMembers :: String
pyAntlrHeader = render $ absVcat generateParserHeader
pyAntlrMembers = render $ absVcat generateParserMembers
