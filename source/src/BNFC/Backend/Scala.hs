module BNFC.Backend.Scala where

import Prelude hiding ((<>))

import BNFC.Backend.Base (mkfile, Backend)
import BNFC.CF
import BNFC.Options hiding (Backend)
import BNFC.PrettyPrint (vcat, Doc)
import BNFC.Backend.Scala.CFtoScalaAbs (cf2ScalaAbs)
import BNFC.Backend.Scala.CFtoScalaLex (cf2ScalaLex)
import BNFC.Backend.Scala.CFtoScalaLexToken (cf2ScalaLexToken)
import BNFC.Backend.Scala.CFtoScalaParser (cf2ScalaParser)
import BNFC.Backend.Scala.CFtoScalaParserAST (cf2ScalaParserAST)

-- | Entrypoint for the Scala backend.

makeScala :: SharedOptions -> CF -> Backend
makeScala opts cf = do 
    mkfile (name ++ ".scala") comment $ cf2ScalaAbs opts cf
    mkfile (name ++ "LexToken.scala") comment $ cf2ScalaLexToken opts cf
    mkfile (name ++ "Lex.scala") comment $ cf2ScalaLex opts cf
    mkfile (name ++ "Parser.scala") comment $ cf2ScalaParser opts cf
    mkfile (name ++ "ParserAST.scala") comment $ cf2ScalaParserAST opts cf
    where name = lang opts

comment :: String -> String
comment = ("// " ++)
