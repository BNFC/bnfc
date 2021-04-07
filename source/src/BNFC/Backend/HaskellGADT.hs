{-
    BNF Converter: Haskell main file
    Copyright (C) 2004-2005  Author:  Markus Forsberg, Peter Gammie,
                                      Aarne Ranta, Björn Bringert

-}

module BNFC.Backend.HaskellGADT (makeHaskellGadt) where


-- import Utils
import BNFC.Options
import BNFC.Backend.Base hiding (Backend)
import BNFC.Backend.Haskell.HsOpts
import BNFC.CF
import BNFC.Backend.Haskell.CFtoHappy
import BNFC.Backend.Haskell.CFtoAlex3
import BNFC.Backend.HaskellGADT.CFtoAbstractGADT
import BNFC.Backend.HaskellGADT.CFtoTemplateGADT
import BNFC.Backend.Haskell.CFtoPrinter
import BNFC.Backend.Haskell.CFtoLayout
import BNFC.Backend.XML
import BNFC.Backend.Haskell.MkErrM
import qualified BNFC.Backend.Common.Makefile as Makefile
import qualified BNFC.Backend.Haskell as Haskell

import Control.Monad(when)


makeHaskellGadt :: SharedOptions -> CF -> MkFiles ()
makeHaskellGadt opts cf = do
  let absMod = absFileM opts
      composOpMod = composOpFileM opts
      lexMod = alexFileM opts
      parMod = happyFileM opts
      prMod  = printerFileM opts
      layMod = layoutFileM opts
      errMod = errFileM opts
  do
    mkfile (absFile opts) $ cf2Abstract (tokenText opts) absMod cf composOpMod
    mkfile (composOpFile opts) $ composOp composOpMod
    case alexMode opts of
      Alex3 -> do
        mkfile (alexFile opts) $ cf2alex3 lexMod (tokenText opts) cf
        liftIO $ putStrLn "   (Use Alex 3 to compile.)"
    mkfile (happyFile opts) $
      cf2Happy parMod absMod lexMod (glr opts) (tokenText opts) False cf
    liftIO $ putStrLn "   (Tested with Happy 1.15 - 1.20)"
    mkfile (templateFile opts) $ cf2Template (templateFileM opts) absMod cf
    mkfile (printerFile opts)  $ cf2Printer StringToken False True prMod absMod cf
    when (hasLayout cf) $ mkfile (layoutFile opts) $
      cf2Layout (tokenText opts) layMod lexMod cf
    mkfile (tFile opts)        $ Haskell.testfile opts cf
    mkfile (errFile opts) $ mkErrM errMod
    Makefile.mkMakefile opts $ Haskell.makefile opts cf
    case xml opts of
      2 -> makeXML opts True cf
      1 -> makeXML opts False cf
      _ -> return ()

composOp :: String -> String
composOp composOpMod = unlines
    [
     "{-# LANGUAGE Rank2Types, PolyKinds #-}",
     "module " ++ composOpMod ++ " (Compos(..),composOp,composOpM,composOpM_,composOpMonoid,",
     "                 composOpMPlus,composOpFold) where",
     "",
     "import Prelude",
     "",
     "import Control.Monad.Identity",
     "",
     "class Compos t where",
     "  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)",
     "         -> (forall a. t a -> m (t a)) -> t c -> m (t c)",
     "",
     "composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c",
     "composOp f = runIdentity . composOpM (Identity . f)",
     "",
     "composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)",
     "composOpM = compos return ap",
     "",
     "composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()",
     "composOpM_ = composOpFold (return ()) (>>)",
     "",
     "composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m",
     "composOpMonoid = composOpFold mempty mappend",
     "",
     "composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b",
     "composOpMPlus = composOpFold mzero mplus",
     "",
     "composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b",
     "composOpFold z c f = unC . compos (\\_ -> C z) (\\(C x) (C y) -> C (c x y)) (C . f)",
     "",
     "newtype C b a = C { unC :: b }"
    ]
