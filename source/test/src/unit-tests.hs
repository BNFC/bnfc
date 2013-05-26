import Test.Hspec

import BNFC.Backend.Common.MakefileSpec
import BNFC.WarningMSpec
import BNFC.OptionsSpec
import BNFC.Backend.Latex.Test
import BNFC.Backend.Haskell.Test

main :: IO ()
main = hspec $ do
  BNFC.Backend.Common.MakefileSpec.spec
  BNFC.WarningMSpec.spec
  BNFC.OptionsSpec.spec
  BNFC.Backend.Latex.Test.spec
  BNFC.Backend.Haskell.Test.spec
