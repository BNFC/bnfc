{-# OPTIONS -Wunused-imports #-}

{-
    BNF Converter: Main file
    Copyright (C) 2002-2013  Authors:
    Jonas Almström Duregård, Krasimir Angelov, Björn Bringert, Johan Broberg, Paul Callaghan,
    Grégoire Détrez, Markus Forsberg, Ola Frid, Peter Gammie, Thomas Hallgren, Patrik Jansson,
    Kristofer Johannisson, Antti-Juhani Kaijanaho, Ulf Norell,
    Michael Pellauer and Aarne Ranta 2002 - 2013.

    Björn Bringert, Johan Broberg, Markus Forsberg, Peter Gammie,
    Patrik Jansson, Antti-Juhani Kaijanaho, Ulf Norell,
    Michael Pellauer, Aarne Ranta

-}

module Main where

import BNFC.Backend.Base ( writeFiles, Backend )
import BNFC.Backend.C ( makeC )
import BNFC.Backend.CPP.NoSTL ( makeCppNoStl )
import BNFC.Backend.CPP.STL ( makeCppStl )
import BNFC.Backend.Haskell ( makeHaskell )
import BNFC.Backend.HaskellGADT ( makeHaskellGadt )
import BNFC.Backend.Java ( makeJava )
import BNFC.Backend.Latex ( makeLatex )
import BNFC.Backend.OCaml ( makeOCaml )
import BNFC.Backend.Pygments ( makePygments )
import BNFC.Backend.TreeSitter ( makeTreeSitter )
import BNFC.CF ( CF )
import BNFC.GetCF ( parseCF )
import BNFC.Options ( Mode(..), Target(..), SharedOptions(..)
                   , parseMode, usage, help, title )

import BNFC.License ( license )
import Paths_BNFC ( version )

import Data.Version ( showVersion )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( stderr, hPutStrLn )

-- Print an error message and a (short) usage help and exit
printUsageErrors :: [String] -> IO ()
printUsageErrors msg = do
  mapM_ (hPutStrLn stderr) msg
  hPutStrLn stderr usage
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  let (mode, warnings) = parseMode args

  -- Print command-line argument warnings (if any).
  mapM_ (hPutStrLn stderr) warnings

  case mode of

    UsageError e   -> printUsageErrors [e]
    Help           -> putStrLn help >> exitSuccess
    Version        -> mapM_ putStrLn title >> exitSuccess
    NumericVersion -> putStrLn (showVersion version) >> exitSuccess
    License        -> putStr license >> exitSuccess

    Target options file
      | target options == TargetCheck ->
          readFile file
            >>= parseCF options TargetCheck
            >>  return ()
      | otherwise ->
          readFile file
            >>= parseCF options (target options)
            >>= writeFiles (outDir options) . maketarget (target options) options

maketarget :: Target -> SharedOptions -> CF -> Backend
maketarget = \case
    TargetC            -> makeC
    TargetCpp          -> makeCppStl
    TargetCppNoStl     -> makeCppNoStl
    TargetHaskell      -> makeHaskell
    TargetHaskellGadt  -> makeHaskellGadt
    TargetLatex        -> makeLatex
    TargetJava         -> makeJava
    TargetOCaml        -> makeOCaml
    TargetPygments     -> makePygments
    TargetCheck        -> error "impossible"
    TargetTreeSitter   -> makeTreeSitter
