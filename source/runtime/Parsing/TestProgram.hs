{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Parsing.TestProgram where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import GHC.Exts
import Control.Monad
import Control.Applicative (pure)
import Parsing.Chart hiding (fingerprint,mkTree)
import Data.Matrix.Quad
import Data.Pair
import Algebra.RingUtils

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()


mainTest :: forall category token. 
            (RingP [(category,Any)]) =>
         ((category,Any) -> String) ->
         (token -> Pair [(category,Any)]) ->
         (String -> [token]) ->
         (token -> (Int,Int)) -> 
         (category -> String) -> 
         IO ()
mainTest showAst cnfToksToCat myLLexer getTokPos describe = 
  do args <- getArgs
     case args of
       [] -> hGetContents stdin >>= run 2
       "-s":fs -> mapM_ (runFile 0) fs
       fs -> mapM_ (runFile 2) fs
       
 where 
  showResults :: [(category,Any)] -> IO ()
  showResults x = do
        putStrLn $ show (length x) ++ " results"
        forM_ x $ \(cat,ast) -> do
          putStrLn $ describe cat        
          putStrLn $ showAst (cat,ast)
  
  runFile v f = putStrLn f >> readFile f >>= run v
  run v s = 
    do case rs of
         [(_,x,_)] -> showResults x
         _ -> do let s = minimum $ map resSz rs
                 mapM_ putStrLn $ showErrs ts s rs
       writeFile "cnf.xpm" (genXPM $ fingerprint chart)
    where ts = myLLexer s
          chart = mkTree $ map cnfToksToCat ts 
          rs = results chart
  
  showTokPos :: (Int,Int) -> String
  showTokPos (l,c) = show l ++ "," ++ show c

  showPos :: [token] -> Int -> String
  showPos ts x = showTokPos (getTokPos $ ts !! x)
  
  showErr ts [(_,x,p),(_,y,_)] = showPos ts p ++ ": cannot combine " ++ showBestCat x ++ " with " ++ showBestCat y

  showErrs ts s [] = []
  showErrs ts s [_] = []
  showErrs ts s (x:y:ys)  = showErr ts [x,y] : showErrs ts s (y:ys)
--    | resSz x == s = showErr ts [x,y] : showErrs ts s (y:ys)
--    | resSz y == s = showErr ts [x,y] : showErrs ts s (y:ys)
--    | otherwise = showErrs ts s (y:ys)


  showBestCat ((x,_):_) = describe x


resSz (i,_,j) = j-i




