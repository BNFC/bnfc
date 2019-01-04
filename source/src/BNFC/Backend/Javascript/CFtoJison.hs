module BNFC.Backend.Javascript.CFtoJison (cf2Jison) where

import Data.List
import Data.Char
import Data.Maybe
import Text.Printf
import Control.Monad.Reader

import BNFC.CF
import BNFC.Backend.Common.NamedVariables

type Group = (Cat, [Rule])
type JisonEnv = (CF, SymEnv)

cf2Jison :: String -> CF -> SymEnv -> String
cf2Jison _ cf env = runReader jison (cf, env)

jison :: Reader JisonEnv String
jison = do
    entry <- jisonEntry
    rules <- asks fst >>= jisonGroups . ruleGroups
    tokens <- asks fst >>= jisonTokens . map show . (map fst) . tokenPragmas
    return $ "%%\n\n" ++ entry ++ "\n\n" ++ rules ++ "\n\n" ++ tokens

jisonEntry :: Reader JisonEnv String
jisonEntry = do
    cs <- asks fst >>= return . cf2Entry
    let entries = intercalate ("\n\t| " :: String) $ map (\c -> printf "%s EOF\n\t\t{{ $$ = $1; return $$ }}" (show c)) cs
    return $ printf "_Entry\n\t: %s \n\t;" entries
    where cf2Entry cf = case (allCats cf) == (allEntryPoints cf) of
            True -> [firstEntry cf] -- Only use the first entry point, jison isnt playing nice otherwise.
            False -> allEntryPoints cf

jisonGroups :: [Group] -> Reader JisonEnv String
jisonGroups gs = interMapM "\n\n" jisonGroup gs

jisonGroup :: Group -> Reader JisonEnv String
jisonGroup (cat, rs) = do
    s <- interMapM "\n\t| " jisonRule rs
    return $ printf "%s\n\t: %s\n\t;" (identCat cat) s

jisonRule :: Rule -> Reader JisonEnv String
jisonRule r = do
    b <- isRev r
    let r' = if b then revSepListRule r else r
    let (_, (_, cs')) = unRule r'
    rule <- unwordsMapM jisonSymbol cs'
    action <- jisonAction r'
    return $ printf "%s\n\t\t%s" rule action

jisonAction :: Rule -> Reader JisonEnv String
jisonAction r
    | isList cat     = jisonActionList r
    | isCoercion fun = jisonActionCoercion r
    | otherwise      = jisonActionNormal r
    where (fun, (cat, _)) = unRule r

jisonActionNormal :: Rule -> Reader JisonEnv String
jisonActionNormal r = do
        return $ printf "{{ $$ = { type : '%s', loc: %s, args : [%s] }; }}"
                  fun jisonLoc (formatArgs cs 1)
    where (fun, (_, cs)) = unRule r

jisonActionCoercion :: Rule -> Reader JisonEnv String
jisonActionCoercion r = return $ printf "{{ $$ = %s; }}" (formatArgs cs 1)
    where (_, (_, cs)) = unRule r

jisonActionList :: Rule -> Reader JisonEnv String
jisonActionList r
    | isNilFun fun  = return "{{ $$ = []; }}"
    | isOneFun fun  = return "{{ $$ = [$1]; }}"
    | isConsFun fun = isRev r >>= \b -> case b of
          True -> return $ printf "{{ $$ = $1.concat([%s]); }}" (formatArgs (tail cs) 2)
          False -> return $ printf "{{ $$ = [$1].concat(%s); }}" (formatArgs (tail cs) 2)
    | otherwise = error "unknown rule"
    where (fun, (_, cs)) = unRule r

jisonSymbol :: Either Cat String -> Reader JisonEnv String
jisonSymbol (Left cat)
    | (show cat) `elem` specialCatsP = return $ map toUpper (show cat)
    | otherwise = return $ identCat cat
jisonSymbol (Right s) = asks ((lookup s) . snd) >>= return . fromJust

jisonTokens :: [String] -> Reader JisonEnv String
jisonTokens ts = interMapM "\n\n" jisonToken ts

jisonToken :: String -> Reader JisonEnv String
jisonToken t = do
    env <- asks snd
    let s = fromJust $ lookup t env
    return $ printf
              "%s\n\t: %s\n\t\t{{ $$ = { type : '%s', loc: %s, args : [$1] }; }}\n\t;"
              t s t jisonLoc

jisonLoc :: String
jisonLoc = printf "{ start: %s, end: %s }" (l "first") (l "last")
    where l s = (printf "{ line: @$.%s_line, column: @$.%s_column }" (s :: String) s) :: String

-- Helpers
isRev r = do
    cf <- asks fst
    return $ (isConsFun fun) && cat `elem` (reversibleCats cf)
    where (fun, (cat, _)) = unRule r
unwordsMapM f l = mapM f l >>= return . unwords
interMapM j f l = mapM f l >>= \s -> return $ intercalate j s
unRule r = (funRule r, (valCat r, rhsRule r))
formatArgs cs n = intercalate ", " args
    where argString arg = "$" ++ (show arg)
          args = filter ((/=) "0")
            $ map (\(n, e) -> either (const (argString n)) (const "0") e)
            $ zip [n..] cs
