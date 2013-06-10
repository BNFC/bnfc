# What is the BNF Converter?

The BNF Converter is a compiler construction tool generating a compiler front-end from a Labelled BNF grammar. It is currently able to generate C, C++, C#, Haskell, Java, and OCaml, as well as XML representations.

Given a Labelled BNF grammar the tool produces:
- an abstract syntax implementation
- a case skeleton for the abstract syntax in the same language
- an Alex, JLex, or Flex lexer generator file
- a Happy, CUP, or Bison parser generator file
- a pretty-printer as a Haskell/Java/C++/C module
- a Latex file containing a readable specification of the language

*More information*: http://bnfc.digitalgrammars.com/

[![Build Status](https://travis-ci.org/BNFC/bnfc.png?branch=master)](https://travis-ci.org/BNFC/bnfc)
