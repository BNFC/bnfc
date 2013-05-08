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
            (RingP [(category,Any)], Eq category) =>
         ((category,Any) -> String) ->
         (Bool -> token -> Pair [(category,Any)]) ->
         (String -> [token]) ->
         (token -> (Int,Int)) ->
         (category -> String) ->
         (category -> [category]) ->
         IO ()
mainTest showAst cnfToksToCat myLLexer getTokPos describe follows =
  do args <- getArgs
     case args of
       [] -> hGetContents stdin >>= run "stdin" 2
       "-s":fs -> mapM_ (runFile 0) fs
       fs -> mapM_ (runFile 2) fs

 where
  neighbors a b = b `elem` follows a
  showResults :: [(category,Any)] -> IO ()
  showResults x = do
        putStrLn $ show (length x) ++ " results"
        forM_ x $ \(cat,ast) -> do
          putStrLn $ describe cat
          putStrLn $ showAst (cat,ast)

  runFile v f = putStrLn f >> readFile f >>= run f v
  run f v s =
    do case rs of
         [(_,x,_)] -> showResults x
         _ -> do let errs = pairs rs
                     best = minimum $ map quality errs
                 mapM_ (putStrLn . showErr ts) $ filter (\x -> quality x == best) errs
       when (v >= 2) $ do
         writeFile (f ++ ".xpm") (genXPM $ fingerprint chart)
         let scatt = scatterplot chart
         putStrLn $ "Scatterplot data size:" ++ show (length scatt)
         writeFile (f ++ ".data") scatt
    where ts = myLLexer s
          chart = mkTree $ zipWith cnfToksToCat (cycle [False,True]) ts
          rs = results chart

  showTokPos :: (Int,Int) -> String
  showTokPos (l,c) = show l ++ "," ++ show (c-1)

  showPos :: [token] -> Int -> String
  showPos ts x = showTokPos (getTokPos $ ts !! x)

  showErr ts ((_,x',p),(_,y',_)) =
     showPos ts p ++ ": cannot combine " ++ showBestCat x' ++ " with " ++ showBestCat y'

  quality (a@(_,x',p),b@(_,y',_)) = (or [ neighbors x y | x <- map fst x', y <- map fst y'],
                                     (resSz a) Prelude.+ (resSz b))


  showBestCat ((x,_):_) = describe x

pairs (x:y:xs) = (x,y):pairs (y:xs)
pairs _ = []

resSz (i,_,j) = j-i




