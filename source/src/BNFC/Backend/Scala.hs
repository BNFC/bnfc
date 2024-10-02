module BNFC.Backend.Scala where

import Prelude hiding ((<>))

import BNFC.Backend.Base (mkfile, Backend)
import BNFC.CF
import BNFC.Options hiding (Backend)
import BNFC.PrettyPrint (vcat, Doc)
import BNFC.Backend.Scala.CFtoScalaAbs (cf2ScalaAbs)

-- | Entrypoint for the Scala backend.

makeScala :: SharedOptions -> CF -> Backend
makeScala opts cf = do 
    mkfile (name ++ ".lx") comment (cf2ScalaAbs cf)
    where name = lang opts

comment :: String -> String
comment = ("// " ++)
