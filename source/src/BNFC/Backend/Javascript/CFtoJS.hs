module BNFC.Backend.Javascript.CFtoJS (jSTest) where

import Text.Printf

jSTest :: String -> String
jSTest name = unlines $ [
        printf "var parser = require('./Parser%s').parser;" name
        , "var fs = require('fs');"
        , printf "var Printer = require('./Printer%s').Visitor;\n" name
        , "function abstractTree(file) {"
        , "\tvar input = fs.readFileSync(file, 'utf-8');"
        , "\tvar tree = parser.parse(input);"
        , "\tconsole.log(JSON.stringify(tree));"
        , "}\n"
        , "function normalizedTree(file) {"
        , "\tvar input = fs.readFileSync(file, 'utf-8');"
        , "\tvar tree = parser.parse(input);"
        , "\tvar printer = new Printer()"
        , "\tprinter.visit(tree);"
        , "\tconsole.log(printer.text);"
        , "}\n"
        , "console.log('[Abstract Tree]\\n');"
        , "abstractTree(process.argv[2]);"
        , "console.log('\\n');"
        , "console.log('[Linearized Tree]\\n');"
        , "normalizedTree(process.argv[2]);"
    ]
