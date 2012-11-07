module CNFTop where

import CF
import GetCF
import Utils

import ToCNF

import Data.Char
import Data.Maybe (fromMaybe,maybe)
import System.Exit (exitFailure)
import Control.Monad(when)


--- YAgrg
data Options = Options 
    { 
     make :: Bool,
--      alex1 :: Bool,
--      alexMode :: AlexMode,
     inDir :: Bool,
     shareStrings :: Bool,
     byteStrings :: Bool,
--     glr :: HappyMode,
     xml :: Int,
     inPackage :: Maybe String,
     lang :: String,
     multi :: Bool
    }



makeCNF :: Bool -> alexMode -> Bool -> Bool -> Bool -> Bool -> Int 
	   -> Maybe String -- ^ The hierarchical package to put the modules
	                   --   in, or Nothing.
	   -> String -> Bool -> FilePath -> IO ()
makeCNF m am d ss bs g x p n mu file = do
  let opts = Options { make = m, inDir = d, shareStrings = ss, byteStrings=bs,
 		       -- glr = if g then GLR else Standard, 
                       xml = x, 
 		       inPackage = p, lang = n, multi = mu}
--       absMod = absFileM opts
--       lexMod = alexFileM opts
--       parMod = happyFileM opts
--       prMod  = printerFileM opts
--       layMod = layoutFileM opts
--       errMod = errFileM opts
--       shareMod = shareFileM opts
  (cf, isOK) <- tryReadCF [formatOptHaskell] file
  if isOK then do
    let (cf',units) = toCNF cf
--     let dir = codeDir opts
--     when (not (null dir)) $ do
-- 			    putStrLn $ "Creating directory " ++ dir
-- 			    prepareDir dir
--     writeFileRep (absFile opts) $ cf2Abstract (byteStrings opts) absMod cf
--     case alexMode opts of
--       Alex1 -> do
--         writeFileRep (alexFile opts) $ cf2alex lexMod errMod cf
--         putStrLn "   (Use Alex 1.1 to compile.)" 
--       Alex2 -> do
--         writeFileRep (alexFile opts) $ cf2alex2 lexMod errMod shareMod (shareStrings opts) (byteStrings opts) cf
--         putStrLn "   (Use Alex 2.0 to compile.)"
--       Alex3 -> do
--         writeFileRep (alexFile opts) $ cf2alex3 lexMod errMod shareMod (shareStrings opts) (byteStrings opts) cf
--         putStrLn "   (Use Alex 3.0 to compile.)"
--     writeFileRep (happyFile opts) $ 
-- 		 cf2HappyS parMod absMod lexMod errMod (glr opts) (byteStrings opts) cf
--     putStrLn "   (Tested with Happy 1.15)"
--     writeFileRep (latexFile opts)    $ cfToLatex (lang opts) cf
--     writeFileRep (txtFile opts)      $ cfToTxt (lang opts) cf
--     writeFileRep (templateFile opts) $ cf2Template (templateFileM opts) absMod errMod cf
--     writeFileRep (printerFile opts)  $ cf2Printer (byteStrings opts) prMod absMod cf
--     when (hasLayout cf) $ writeFileRep (layoutFile opts) $ cf2Layout (alexMode opts == Alex1) (inDir opts) layMod lexMod cf
--     writeFileRep (tFile opts)        $ testfile opts cf
--     writeFileRep (errFile opts)      $ errM errMod cf
--    when (shareStrings opts) $ writeFileRep (shareFile opts)    $ sharedString shareMod (byteStrings opts) cf
--    when (make opts) $ writeFileRep "Makefile" $ makefile opts
--     case xml opts of
--       2 -> makeXML (lang opts) True cf
--       1 -> makeXML (lang opts) False cf
--       _ -> return ()
    putStrLn "ORIGINAL --------------"              
    print cf
    putStrLn "2NF ---------------"              
    print cf'
    putStrLn "CODE ==============" 
    writeFile "ParseTables.hs" $ generate file $ (cf',units)
    putStrLn $ "Done!"
   else do putStrLn $ "Failed!"
	   exitFailure
