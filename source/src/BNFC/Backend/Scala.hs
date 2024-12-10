module BNFC.Backend.Scala where

import Prelude hiding ((<>))

import BNFC.Backend.Base (mkfile, Backend)
import BNFC.CF
import BNFC.Options hiding (Backend)
import BNFC.PrettyPrint (vcat, Doc)
import BNFC.Backend.Scala.CFtoScalaAbs (cf2ScalaAbs)
import BNFC.Backend.Scala.CFtoScalaLex (cf2ScalaLex)
import BNFC.Backend.Scala.CFtoScalaLexToken (cf2ScalaLexToken)

-- | Entrypoint for the Scala backend.

makeScala :: SharedOptions -> CF -> Backend
makeScala opts cf = do 
    mkfile (name ++ ".scala") comment $ cf2ScalaAbs opts cf
    mkfile (name ++ "LexToken.scala") comment $ cf2ScalaLexToken opts cf
    mkfile (name ++ "Lex.scala") comment $ cf2ScalaLex opts cf
    where name = lang opts

comment :: String -> String
comment = ("// " ++)
