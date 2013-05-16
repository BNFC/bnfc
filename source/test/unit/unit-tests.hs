import Test.Hspec

import BNFC.Backend.Common.MakefileSpec
import BNFC.Backend.LatexSpec
import BNFC.WarningMSpec
import BNFC.OptionsSpec

main :: IO ()
main = hspec $ do
  BNFC.Backend.Common.MakefileSpec.spec
  BNFC.Backend.LatexSpec.spec
  BNFC.WarningMSpec.spec
  BNFC.OptionsSpec.spec
