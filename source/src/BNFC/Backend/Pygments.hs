{-# LANGUAGE NoImplicitPrelude #-}

{- Generates a Pygments lexer from a BNF grammar.
 -
 - Resources:
 - * Pygments: http://pygments.org/
 - * Lexer development: http://pygments.org/docs/lexerdevelopment/
 - * Token types: http://pygments.org/docs/tokens/
 - -}
module BNFC.Backend.Pygments where

import Prelude'

import AbsBNF (Reg(..))
import BNFC.Backend.Base (mkfile, Backend)
import BNFC.CF
import BNFC.Lexing
import BNFC.Options hiding (Backend)
import BNFC.Utils
import BNFC.PrettyPrint

makePygments :: SharedOptions -> CF -> Backend
makePygments opts cf = do
    let lexerfile = render (lowerCase name <> "/__init__.py")
        setupfile = "setup.py"
    mkfile lexerfile (lexer name cf)
    mkfile setupfile (setup name)
  where name = lang opts

setup :: String -> Doc
setup name = vcat
    [ "from setuptools import setup, find_packages"
    , "setup" <> parens (fsep (punctuate ","
        [ "name" <=> quotes ("pygment-"<>lowerCase name)
        , "version" <=> "0.1"
        , "packages" <=> brackets (quotes moduleName)
        , "entry_points" <=> entryPoints
        , "install_requires = ['pygments']"
        ]))
    ]
  where
    className = camelCase name <> "Lexer"
    moduleName = lowerCase name
    entryPoints =
        braces( "'pygments.lexers':"
              <> doubleQuotes (moduleName <> "=" <> moduleName <> ":" <> className))

lexer :: String -> CF -> Doc
lexer name cf = vcat
    -- Import statments
    [ "import pygments.lexer"
    , "from pygments.token import *"
    -- Declare our lexer
    , "__all__" <=> brackets (doubleQuotes className)
    -- define lexer
    , "class" <+> className <> parens "pygments.lexer.RegexLexer" <> ":"
    , indent
        [ "name" <=> quotes (text name)
        , "aliases" <=> brackets (quotes (lowerCase name))
        -- filenames = ['*.cf', '*lbnf']
        , "KEYWORDS" <=> brackets keywords
        -- We override the get_tokens_unprocessed method to filter keywords
        -- from identifiers
        , "def get_tokens_unprocessed(self, text):"
        , indent
            [ "for index, token, value in super(" <> className <> ",self).get_tokens_unprocessed(text):"
            , indent
                [ "if token is Name and value in self.KEYWORDS:"
                , indent [ "yield index, Keyword, value" ]
                , "else:"
                , indent [ "yield index, token, value" ]
                ]
            ]
        -- The token is defined using regex
        , "tokens = {"
        , indent
            [ "'root': ["
            , indent (map prLexRule (mkLexer cf) ++ ["(r'\\s+', Token.Space)"])
            , "]"
            ]
        , "}"
        ]
    ]
  where
    className = camelCase name <> "Lexer"
    keywords = fsep (punctuate "," (map (quotes . text) (reservedWords cf)))
    indent = nest 4 . vcat
    prLexRule (reg,ltype) =
        parens ("r" <> quotes (pyRegex reg) <> "," <+> pyToken ltype) <> ","
    pyToken LexComment = "Comment"
    pyToken LexSymbols = "Operator"
    pyToken (LexToken "Integer") = "Number.Integer"
    pyToken (LexToken "Double") = "Number.Float"
    pyToken (LexToken "Char") = "String.Char"
    pyToken (LexToken "String") = "String.Double"
    pyToken (LexToken _) = "Name"



-- | Convert a Reg to a python regex
-- >>> pyRegex (RSeqs "abc")
-- abc
-- >>> pyRegex (RAlt (RSeqs "::=") (RChar '.'))
-- ::=|\.
-- >>> pyRegex (RChar '=')
-- =
-- >>> pyRegex RAny
-- .
-- >>> pyRegex (RStar RAny)
-- .*
-- >>> pyRegex (RPlus (RSeqs "xxx"))
-- (xxx)+
-- >>> pyRegex (ROpt (RSeqs "abc"))
-- (abc)?
-- >>> pyRegex (RSeq (RSeqs "--") (RSeq (RStar RAny) (RChar '\n')))
-- --.*\n
-- >>> pyRegex (RStar (RSeq (RSeqs "abc") (RChar '*')))
-- (abc\*)*
-- >>> pyRegex REps
-- <BLANKLINE>
-- >>> pyRegex (RAlts "abc[].")
-- [abc\[\]\.]
-- >>> pyRegex RDigit
-- \d
-- >>> pyRegex RLetter
-- [a-zA-Z]
-- >>> pyRegex RUpper
-- [A-Z]
-- >>> pyRegex RLower
-- [a-z]
-- >>> pyRegex (RMinus RAny RDigit)
-- (.)(?<!\d)
-- >>> pyRegex (RSeq (RAlt (RChar 'a') RAny) (RAlt (RChar 'b') (RChar 'c')))
-- (a|.)(b|c)
pyRegex :: Reg -> Doc
pyRegex reg = case reg of
    RSeqs s       -> text (concatMap escape s)
    RAlt r1 r2    -> pyRegex r1 <> "|" <> pyRegex r2
    RChar c       -> text (escape c)
    RAny          -> char '.'
    RStar RAny    -> ".*"
    RStar re      -> parens (pyRegex re) <> char '*'
    RPlus re      -> parens (pyRegex re) <> char '+'
    ROpt re       -> parens (pyRegex re) <> char '?'
    RSeq r1 r2    -> pyRegex' r1 <> pyRegex' r2
    REps          -> empty
    RAlts cs      -> brackets (hcat (map (pyRegex . RChar) cs))
    RDigit        -> "\\d"
    RUpper        -> "[A-Z]"
    RLower        -> "[a-z]"
    RLetter       -> "[a-zA-Z]"
    RMinus r1 r2  -> parens (pyRegex r1) <> parens ("?<!" <> pyRegex r2)
  where
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape c | c `elem` (".'[]()|*+?{}\\" :: String) = ['\\',c]
    escape c = [c]
    pyRegex' r@(RAlt{}) = parens (pyRegex r)
    pyRegex' r = pyRegex r

