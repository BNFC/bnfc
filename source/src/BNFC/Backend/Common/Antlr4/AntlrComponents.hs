module BNFC.Backend.Common.Antlr4.AntlrComponents(lexerMembersContent, 
                                                parserMembersContent, 
                                                lexerHeaderContent, 
                                                parserHeaderContent) where

header, members :: String -> String -> String
header who x = antlrcomponent who "header" x 
members who x = antlrcomponent who "members" x 
 
lexerMembersContent, parserMembersContent, lexerHeaderContent, parserHeaderContent :: String -> String
lexerHeaderContent x = header "lexer" x
parserHeaderContent x = header "parser" x
lexerMembersContent x = members "lexer" x
parserMembersContent x = members "parser" x

antlrcomponent :: String -> String -> String -> String
antlrcomponent who what x = "@"++who++"::"++what++"{"++x++"}"