{-
    BNF Converter: Abstract syntax Generator
    Copyright (C) 2016  Author:  Pascal Hof

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

module BNFC.Backend.Haskell.CFtoCabal (cf2Cabal) where

import Distribution.Simple hiding (Language)
import Distribution.ModuleName(ModuleName,fromString)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse(showPackageDescription)

import BNFC.Backend.Haskell.HsOpts
import BNFC.Options hiding (Version)
import Data.Monoid(mempty)

-- to produce a Cabal file
cf2Cabal :: SharedOptions -> String
cf2Cabal = showPackageDescription . buildPackageDescription

buildPackageDescription :: SharedOptions -> PackageDescription
buildPackageDescription opt = emptyPackageDescription
  { package =
    PackageIdentifier
      { pkgName = PackageName (lang opt)
      , pkgVersion = Version [0,1] []}
  , library =
    Just mempty
      { exposedModules = exposedMods opt
      , libExposed = True
      , libBuildInfo = emptyBuildInfo
          { buildable    = True
          , hsSourceDirs = ["."]
          , targetBuildDepends = dependencies opt
          }
      }
  , extraSrcFiles = [ happyFile opt, alexFile opt ]
  , buildType = Just Simple
  , license = AllRightsReserved
   }

dependencies :: SharedOptions -> [Dependency]
dependencies opt =
    [ Dependency (PackageName "base") (laterVersion $ Version [4] [])
    , Dependency (PackageName "array") anyVersion
    ] ++
    [ Dependency (PackageName "mtl") anyVersion | TargetHaskellGadt == target opt ]


-- |returns a list of all exposed modules
exposedMods :: SharedOptions -> [ModuleName]
exposedMods opt = map fromString $
  [absFileM opt
  ,errFileM opt
  ,printerFileM opt
  ,alexFileM opt
  ,happyFileM opt
  ] ++
  [xmlFileM opt | xml opt > 0] ++
  [composOpFileM opt | TargetHaskellGadt == target opt ]
