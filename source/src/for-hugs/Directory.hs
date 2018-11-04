{-
    BNF Converter: directory handling
    Copyright (C) 2004  Author: Aarne Ranta

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

module Directory where

import System (system)

-- almost-dummy replacement of Directory.hs for Hugs. AR 21/10/2003

doesDirectoryExist :: String -> IO Bool
doesDirectoryExist d = putStrLn ("trying to create " ++ d) >> return False

createDirectory :: String -> IO ()
createDirectory d = system ("mkdir " ++ d) >> return ()
