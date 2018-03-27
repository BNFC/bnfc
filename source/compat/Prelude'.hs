{-# LANGUAGE CPP #-}

-- See #227 for what's going on here.

module Prelude'
(
  module P
)
where

#if __GLASGOW_HASKELL__ >= 803
import Prelude as P hiding ((<>))
#else
import Prelude as P
#endif
