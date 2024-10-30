
{-  
    BNF Converter: Python backend helper functions
    Copyright (C) 2024  Author: Bjorn Werner
-}

module BNFC.Backend.Python.PyHelpers where
import Data.List     ( intercalate )
import Data.Char
import BNFC.CF


addCommas :: [String] -> String
addCommas ss = intercalate ", " ss


addCitationSigns :: String -> String
addCitationSigns ss = "'" ++ ss ++ "'"


filterOut :: Eq a => [a] -> [a] -> [a]
filterOut xs ys = filter (\x -> not (elem x ys)) xs 


-- Converts every character to unicode with an underscore in front.
toOrd :: String -> String
toOrd s = concat (map (("_" ++) . show . ord) s)


-- | Converts a string of underscores and unicode numbers such as "_53_53"
--   into "++".
toChr :: String -> String
toChr "" = ""
toChr xs =  map chr nrs
   where
      nrsStr = tail $ split '_' xs :: [String]
      nrs = map read nrsStr :: [Int]

 
split :: Char -> String -> [String]
split c s = split' c s ""


split' :: Char -> String -> String -> [String]
split' _ [] ps = [ps]
split' c (s:ss) ps
   | c == s = [ps] ++ split' c ss ""
   | otherwise = split' c ss (ps ++ [s])


-- Converts [Cat] into ListCat, which is mainly used in the parser.
translateToList :: String -> String
translateToList s
  | strIsList s = "List" ++ (tail $ init s)
  | otherwise = s


strIsList :: String -> Bool
strIsList s = head s == '[' && last s == ']'


firstRight :: [Either a b] -> Maybe b
firstRight [] = Nothing
firstRight (Left _:es) = firstRight es
firstRight (Right r:_) = Just r


-- Retrieves the first character from strings such as "[Stm]" or "Stm".
firstAlpha :: String -> Char
firstAlpha s
  | strIsList s = head $ tail s
  | otherwise = head s


-- | Converts a production into a string, for comments.
showEcss :: [Either Cat String] -> String
showEcss [] = ""
showEcss (Left c:ecss) = show c ++ " " ++ (showEcss ecss)
showEcss (Right strOp:ecss) = "\"" ++ strOp ++ "\" " ++ (showEcss ecss)

