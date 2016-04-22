module BNFC.Backend.Java.AntlrAdapter(generateAntlrAction) where

import BNFC.CF
import Data.List
import BNFC.Utils ((+.+))
import BNFC.Backend.Common.MultipleParserGenerationTools (ToolParameters (..))

-- Type declarations
type Action      = String
type MetaVar     = (String, Cat)

generateAntlrAction :: ToolParameters -> NonTerminal -> Fun -> [MetaVar]
               -> Bool   -- ^ Whether the list should be reversed or not.
                         --   Only used if this is a list rule.
               -> Action
generateAntlrAction tpar nt f ms rev  
    | isNilFun f = "$result = new " ++ c ++ "();"
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
     c                 = (packageAbsyn tpar)++ "." ++
                            if isNilFun f || isOneFun f || isConsFun f
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