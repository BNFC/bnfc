module BNFC.Backend.Javascript (makeJavascript) where

import Text.Printf

import BNFC.CF
import BNFC.Options

import BNFC.Backend.Base
import BNFC.Backend.Javascript.CFtoJSLex
import BNFC.Backend.Javascript.CFtoJison
import BNFC.Backend.Javascript.CFtoJSPrinter
import BNFC.Backend.Javascript.CFtoJSSkeleton
import BNFC.Backend.Javascript.CFtoJS
import qualified BNFC.Backend.Common.Makefile as Makefile

makeJavascript :: SharedOptions -> CF -> MkFiles ()
makeJavascript opts cf = do
    let (lex, env) = cf2jsLex cf
    mkfile (name ++ ".jisonlex") lex
    mkfile (name ++ ".jison") (cf2Jison name cf env)
    mkfile ("Printer" ++ name ++ ".js") (jSPrinter cf env)
    mkfile ("Skeleton" ++ name ++ ".js") (jSSkeleton cf env)
    mkfile ("Test" ++ name ++ ".js") (jSTest name)
    Makefile.mkMakefile opts (makefile name)
    where name = lang opts

makefile :: String -> String
makefile name = unlines $ [
        printf "Parser%s.js: %s.jison %s.jisonlex" name name name
        , printf "\tjison %s.jison %s.jisonlex -o Parser%s.js" name name name
    ]
