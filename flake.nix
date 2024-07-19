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
        overlays = [ haskellNix.overlay
          (final: _prev: {
            bnfc-project =
              final.haskell-nix.project' {
                src = ./.;
                # name = "project-name";
                compiler-nix-name = "ghc982"; # Version of GHC to use
                # Use the corresponding stack configuration as well
                stackYaml = "stack-9.8.yaml";
                # put your current system for `nix flake show` to work:
                evalSystem = "x86_64-linux";
                # Tools to include in the development shell
                shell.tools = {
                  cabal = "latest";
                  hlint = "latest";
                  haskell-language-server = "lastest";
                };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.bnfc-project.flake {};
      in flake // {
        legacyPackages = pkgs;
        apps.default = flake.apps."BNFC:exe:bnfc";
        packages = rec {
          bnfc = flake.packages."BNFC:exe:bnfc";
          default = bnfc;
          libBNFC = flake.packages."BNFC:lib:BNFC";
        };
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
