{
  description = "A GHC plugin for detecting and instrumenting overloaded function application.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inputs = inputs; } {
    systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
    perSystem = { system, pkgs, lib, ... }: let
      hpkgs = pkgs.haskell.packages.ghc910;
      specialistOverrides = hpkgs: hprev: {
        specialist = hpkgs.developPackage {
          root = ./.;
        };
      };
    in {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          (final: prev: {
            haskell = prev.haskell // {
              packageOverrides = lib.composeExtensions prev.haskell.packageOverrides specialistOverrides;
            };
          })
        ];
      };

      packages = {
        default = hpkgs.specialist;
      };

      devShells.default = hpkgs.shellFor {
        packages = h: [h.specialist];

        nativeBuildInputs = [
          pkgs.cabal-install
          hpkgs.haskell-language-server
        ];
      };

      overlayAttrs.haskell = pkgs.haskell // {
        packageOverrides = lib.composeManyExtensions [
          pkgs.haskell.packageOverrides
          specialistOverrides
        ];
      };
    };

    imports = [ inputs.flake-parts.flakeModules.easyOverlay ];
  };
}
