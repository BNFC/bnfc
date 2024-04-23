-- | Common definitions for the modules of the C backend.

module BNFC.Backend.C.Common
  ( memName
  , posixC
  ) where

import Prelude
import BNFC.Backend.Common.NamedVariables

-- | Switch C to language variant that has @strdup@.

posixC :: [String]
posixC =
  [ "/* strdup was not in the ISO C standard before 6/2019 (C2x), but in POSIX 1003.1."
  , " * See: https://en.cppreference.com/w/c/experimental/dynamic/strdup"
  , " * Setting _POSIX_C_SOURCE to 200809L activates strdup in string.h."
  , " */"
  , "#define _POSIX_C_SOURCE 200809L"
  ]

-- | Variant names in unions.

memName :: String -> String
memName s = firstLowerCase s ++ "_"
