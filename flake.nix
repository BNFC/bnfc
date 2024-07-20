{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    let
      supportedSystems = [
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
            ghcVersion: value: {
              name = "bnfc-project-${ghcVersion}";
              value = final.haskell-nix.stackProject' {
                src = ./.;
                name = "bnfc-project-${ghcVersion}";
                compiler-nix-name = ghcVersion; # Version of GHC to use
                # Use the corresponding stack configuration as well
                stackYaml = value;
                # Tools to include in the development shell
                shell.tools = {
                  cabal = "latest";
                  hlint = {
                    ghc8107 = "3.4";
                    ghc902 = "3.5";
                    ghc928 = "3.6.1";
                    ghc948 = "3.8";
                    ghc965 = "3.8";
                    ghc982 = "3.8";
                  }.${ghcVersion};
                  haskell-language-server = if ghcVersion == "ghc8107" then "2.2.0.0"
                    else if ghcVersion == "ghc902" then "2.4.0.0"
                    else "latest";
                };
                modules = [{
                  # Disable check for doctests
                  packages.BNFC.components.tests.doctests.doCheck = false;
                }];
              # For `nix flake show --impure` and `nix flake check --impure` to work
              } // ( if (builtins ? currentSystem) then {evalSystem = builtins.currentSystem;} else {});
            }) supportedGHCStackFile
          )
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        allVersions = builtins.mapAttrs (
          name: _value: pkgs."bnfc-project-${name}".flake {}
        ) supportedGHCStackFile;

        allFlake = allVersions.${defaultVersion};
        # `nix flake check` currently does not support custom check logics, and will check hydrajobs
        # defined for all platforms, which can cause check failures (see issue NixOS/nix#6453)
        # For `nix flake check` to work, filter out all hydrajobs that are not for the current platform 
        # Note that on non x86_64-linux platforms --impure is required to pick up the correct platform names
        flake = if system == (builtins.currentSystem or "x86_64-linux")
          then allFlake
          else builtins.removeAttrs allFlake ["hydraJobs"];
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
