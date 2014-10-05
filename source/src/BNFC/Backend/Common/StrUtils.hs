module BNFC.Backend.Common.StrUtils where

-- | Function that, given an input string, renders it either as a char (if
-- it has legth 1) or a string. It should also excape characters correctly.
-- The first returned value is the 'type' of the string: either C for char
-- or S for string. (used in the C printer to choose the right rendering
-- function)
-- e.g.
-- >>> renderCharOrString "a"
-- ('C',"'a'")
-- >>> renderCharOrString "abc"
-- ('S',"\"abc\"")
-- >>> renderCharOrString "'"
-- ('C',"'\\''")
-- >>> renderCharOrString "\"\\'"
-- ('S',"\"\\\"\\\\\\'\"")
renderCharOrString :: String -> (Char, String)
renderCharOrString [char] = ('C', show char)     -- using show shoud quote '
renderCharOrString s =('S', "\"" ++ escapeChars s ++ "\"")

-- | Helper function that escapes characters in strings
-- >>> escapeChars "\\"
-- "\\\\"
-- >>> escapeChars "\""
-- "\\\""
-- >>> escapeChars "'"
-- "\\'"
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : '\\' : escapeChars xs
escapeChars ('\"':xs) = '\\' : '\"' : escapeChars xs
escapeChars ('\'':xs) = '\\' : '\'' : escapeChars xs
escapeChars (x:xs) = x : escapeChars xs


