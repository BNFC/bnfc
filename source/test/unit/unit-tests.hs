import Test.Hspec

import BNFC.Backend.Common.MakefileSpec
import BNFC.WarningMSpec
import BNFC.OptionsSpec

main :: IO ()
main = hspec $ do
  BNFC.Backend.Common.MakefileSpec.spec
  BNFC.WarningMSpec.spec
  BNFC.OptionsSpec.spec
