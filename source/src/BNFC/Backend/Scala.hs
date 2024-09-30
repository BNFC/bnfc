module BNFC.Backend.Scala where

import Prelude hiding ((<>))

import BNFC.Backend.Base (mkfile, Backend)
import BNFC.CF
import BNFC.Options hiding (Backend)
import BNFC.PrettyPrint (vcat, Doc)

-- | Entrypoint for the Scala backend.

makeScala :: SharedOptions -> CF -> Backend
makeScala opts cf = do
    mkfile "scala.lx" comment $ vcat [ "ScalaFirt try" ]

comment :: String -> String
comment = ("// " ++)
