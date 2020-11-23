{-
    BNF Converter: directory handling
    Copyright (C) 2004  Author: Aarne Ranta

-}

module Directory where

import System (system)

-- almost-dummy replacement of Directory.hs for Hugs. AR 21/10/2003

doesDirectoryExist :: String -> IO Bool
doesDirectoryExist d = putStrLn ("trying to create " ++ d) >> return False

createDirectory :: String -> IO ()
createDirectory d = system ("mkdir " ++ d) >> return ()
