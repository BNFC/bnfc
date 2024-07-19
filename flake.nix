{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    let
      supportedSystems = [
        # Note: Only Linux on x6_64 is tested to work for now
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        lib = nixpkgs.lib;
        supportedGHCStackFile = {
          # Haskell.nix does not support GHC versions older than 8.10
          # Note: only GHC 9.8.2 is tested to work for now
          ghc8107 = "stack-8.10.yaml";
          ghc902 = "stack-9.0.yaml";
          ghc928 = "stack-9.2.yaml";
          ghc948 = "stack-9.4.yaml";
          ghc965 = "stack-9.6.yaml";
          ghc982 = "stack-9.8.yaml";
        };
        defaultVersion = "ghc982";

        overlays = [ haskellNix.overlay
          (final: _prev: lib.attrsets.mapAttrs' (
            name: value: {
              name = "bnfc-project-${name}";
              value = final.haskell-nix.stackProject' {
                src = ./.;
                # name = "project-name";
                compiler-nix-name = name; # Version of GHC to use
                # Use the corresponding stack configuration as well
                stackYaml = value;
                # put your current system for `nix flake show` to work:
                evalSystem = "x86_64-linux";
                # Tools to include in the development shell
                shell.tools = {
                  cabal = "latest";
                  hlint = "latest";
                  haskell-language-server = "lastest";
                };
              };
            }) supportedGHCStackFile
          )
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        allVersions = builtins.mapAttrs (
          name: _value: pkgs."bnfc-project-${name}".flake {}
        ) supportedGHCStackFile;

        flake = allVersions.${defaultVersion};
      in flake // {
        legacyPackages = pkgs;
        apps = {
          default = flake.apps."BNFC:exe:bnfc";
        } // (lib.attrsets.mapAttrs' (
          name: _value: {
            name = "bnfc-${name}";
            value = allVersions.${name}.apps."BNFC:exe:bnfc";
          }
        ) supportedGHCStackFile);
        packages = rec {
          bnfc = flake.packages."BNFC:exe:bnfc";
          default = bnfc;
          libBNFC = flake.packages."BNFC:lib:BNFC";
        } // (lib.attrsets.mapAttrs' (
          name: _value: {
            name = "bnfc-${name}";
            value = allVersions.${name}.packages."BNFC:exe:bnfc";
          }
        ) supportedGHCStackFile)
        // (lib.attrsets.mapAttrs' (
          name: _value: {
            name = "libBNFC-${name}";
            value = allVersions.${name}.packages."BNFC:lib:BNFC";
          }
        ) supportedGHCStackFile);
        devShells = {
          default = flake.devShells.default;
        } // (lib.attrsets.mapAttrs' (
          name: _value: {
            name = "bnfc-${name}";
            value = allVersions.${name}.devShells.default;
          }
        ) supportedGHCStackFile);
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
