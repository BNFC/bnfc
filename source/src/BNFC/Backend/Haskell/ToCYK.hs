{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-
    Copyright (C) 2014  Authors:
    Tobias Olausson

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module BNFC.Backend.Haskell.ToCYK (generate) where

{-
Generation of an incremental parser for use with the CYK tables and 
incremental lexer. 

This parsing process was described in Olausson, Tobias (2014), "Implementing
incremental and parallel parsing", University of Gothenburg. 
-}

import BNFC.CF hiding (App,Exp)
import BNFC.ToCNFCore
import BNFC.Backend.Haskell.HsOpts
import Data.Monoid ((<>))
import Text.PrettyPrint.HughesPJ hiding (first,(<>))

-- Code generation

-- We don't have to run the CNF conversion since the names of any entrypoints
-- will stay the same anyway (they are top-level definitions).
generate opts cf0 = render $ vcat [header opts cf0
                                  ,measured opts
                                  ,toAST cf0
                                  ]

header opts cf = vcat $ 
    ["{-# LANGUAGE MagicHash, FlexibleInstances, MultiParamTypeClasses #-}"
    ,"module " <> text (incCykFileM opts) <> "("<> exports cf <> ") where"
    ,"import GHC.Prim"
    ,"import GHC.Exts"
    ,"import Control.Applicative"
    ,"import Data.Monoid hiding (Any)"
    ,"import System.IO.Unsafe (unsafePerformIO)"
    ,"import System.Random"
    ,"import Data.FingerTree"
    ,""
    ,"import Algebra.RingUtils"
    ,"import Parsing.Chart ()"
    ,"import Data.Matrix.Quad"
    ,"import " <> text (absFileM  opts)
    ,"import " <> text (alexFileM opts) <> " hiding (One)"
    ,"import " <> text (cnfTablesFileM opts)
    ,""
    ]

measured opts = vcat $
    ["instance RingP a => Monoid (SomeTri a) where"
    ,"  mempty = T Leaf' (Zero :/: Zero)"
    ,"  t0 `mappend` t1 = unsafePerformIO $ do"
    ,"      b <- randomIO"
    ,"      return $ merge b t0 t1"
    ,""
    ,"instance Measured (SomeTri [(CATEGORY,Any)]) IntToken where"
    ,"    -- Note: place the token just above the diagonal"
    ,"    measure tok = T (bin' Leaf' Leaf') (q True :/: q False)"
    ,"      where q b = quad zero (t b) zero zero"
    ,"            select b = if b then leftOf else rightOf"
    ,"            t b = case intToToken tok of"
    ,"                Nothing    -> Zero"
    ,"                Just token -> One $ select b $ tokenToCats b token"
    ,""
    ,"type ParseState = SomeTri [(CATEGORY,Any)]"
    ,"parse :: FingerTree ParseState IntToken -> [(Int,[(CATEGORY,Any)],Int)]"
    ,"parse tree = results $ measure tree"
    ,""
    ,"intToToken :: IntToken -> Maybe Token"
    ,"intToToken (Token lexeme acc) = case acc of"
    ,"    AlexAcc f   -> Just $ f (Pn 0 1 1) lexeme"
    ,"    _           -> Nothing"
    ,""
    ]

-- Create one function for each entrypoint category, so user can decide on which
-- one to use.
toAST cf = vcat $
    "--use these functions to convert from parser-internal state to AST" : 
    [ vcat $ 
      ["get" <> text c <> " :: (CATEGORY,Any) -> Maybe " <> text c
      ,"get" <> text c <> " ("<>catTag (Left c)<>",ast) = Just $ ((unsafeCoerce# ast)::"<> text c <> ")"
      ,"get" <> text c <> " _ = Nothing"
      ,""
      ,"getAll" <> text c <> " :: [(a,[(CATEGORY,Any)],a)] -> [Maybe " <> text c <> "]"
      ,"getAll" <> text c <> " = concatMap (\\(_,cs,_) -> map get" <> text c <> " cs)"
      ,""
      ] | c <- allEntryPoints cf
    ]

-- Make sure all entrypoint functions are exported, together with needed types
exports cf = cat e <> "CATEGORY, Any, parse"
  where e = ["get" <> text c <> ", getAll" <> text c <> "," | c <- allEntryPoints cf]

