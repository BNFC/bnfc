{-
    BNF Converter: Haskell string sharing
    Copyright (C) 2004-2007  Author:  Björn Bringert

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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}
module BNFC.Backend.Haskell.MkSharedString where

sharedString :: String -> Bool -> b -> String
sharedString shareMod byteString _ = unlines $
  if byteString
    then
      [
       "module " ++ shareMod ++ " (shareString) where",
       "",
       "import Data.Map as M",
       "import Data.IORef",
       "import qualified Data.ByteString.Char8 as BS",
       "import System.IO.Unsafe (unsafePerformIO)",
       "",
       "{-# NOINLINE stringPoolRef #-}",
       "stringPoolRef :: IORef (M.Map BS.ByteString BS.ByteString)",
       "stringPoolRef = unsafePerformIO $ newIORef M.empty",
       "",
       "{-# NOINLINE shareString #-}",
       "shareString :: BS.ByteString -> BS.ByteString",
       "shareString s = unsafePerformIO $ do",
       "  stringPool <- readIORef stringPoolRef",
       "  case M.lookup s stringPool of",
       "    Just s' -> return s'",
       "    Nothing -> do let s' = BS.copy s",
       "                  writeIORef stringPoolRef $! M.insert s' s' stringPool",
       "                  return s'"
      ]
    else
      [
       "module " ++ shareMod ++ " (shareString) where",
       "",
       "import Data.HashTable as H",
       "import System.IO.Unsafe (unsafePerformIO)",
       "",
       "{-# NOINLINE stringPool #-}",
       "stringPool :: HashTable String String",
       "stringPool = unsafePerformIO $ new (==) hashString",
       "",
       "{-# NOINLINE shareString #-}",
       "shareString :: String -> String",
       "shareString s = unsafePerformIO $ do",
       "  mv <- H.lookup stringPool s",
       "  case mv of",
       "       Just s' -> return s'",
       "       Nothing -> do",
       "                  H.insert stringPool s s",
       "                  return s"
      ]
