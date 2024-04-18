{
  description = "Track the publish dates of your favorite comics";

  inputs = {
    emacs-overlay = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:nix-community/emacs-overlay";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pre-commit-hooks-nix = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:cachix/pre-commit-hooks.nix";
    };
  };

  outputs = inputs@{ self, emacs-overlay, flake-utils, nixpkgs, ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
      ];

      flake = {
        overlays = {
          default =
            nixpkgs.lib.composeManyExtensions
              (nixpkgs.lib.attrValues
                (nixpkgs.lib.filterAttrs
                  (n: _: n != "default")
                  self.overlays));

          myEmacs = nixpkgs.lib.composeExtensions emacs-overlay.overlay (final: prev: {
            myEmacs = prev.emacsWithPackagesFromUsePackage {
              alwaysEnsure = true;
              config = ./emacs.el;
            };
          });
        };
      };

      systems = [
        "x86_64-linux"
      ];

      perSystem = { config, lib, pkgs, self', system, ... }: {
        _module.args.pkgs = import nixpkgs {
          overlays = [ self.overlays.default ];
          inherit system;
        };

        apps = {
          ComiCal = flake-utils.lib.mkApp {
            drv = pkgs.haskell.lib.justStaticExecutables self'.packages.ComiCal;
          };
          default = self'.apps.ComiCal;
        };

        devShells.default = pkgs.mkShell {
          FONTCONFIG_FILE = with pkgs; makeFontsConf {
            fontDirectories = [
              (nerdfonts.override { fonts = [ "Iosevka" ]; })
            ];
          };

          inputsFrom = [
            # TODO: make this its own chunk and describe it
            self'.packages.ComiCal.env
            config.pre-commit.devShell
          ];

          nativeBuildInputs = with pkgs; [
            cabal-install
            haskellPackages.pointfree
            myEmacs
            ghcid
            haskell-language-server
            noweb
            python3Packages.pygments
            (
              texlive.combine {
                inherit (texlive) scheme-small;
                inherit noweb;
                # tufte-latex and deps
                inherit (texlive)
                  catchfile
                  fmtcount
                  framed
                  fvextra
                  hardwrap
                  mathpazo
                  titlesec
                  tufte-latex
                  xstring
                  ;
                # my preferred packages
                inherit (texlive)
                  datetime
                  dirtytalk
                  fancyref
                  latexmk
                  minted
                  todonotes
                  xetex
                  ;
              }
            )
            python3Packages.pywatchman
            semver-tool
            coreutils
            perl
          ];
        };

        packages = {
          ComiCal = pkgs.haskellPackages.callCabal2nix "ComiCal" self { };
          default = self'.packages.ComiCal;
        };

        pre-commit.settings = {
          hooks = {
            deadnix = {
              enable = true;
              settings.noLambdaArg = true;
            };
            hlint.enable = true;
            make-srcs = {
              always_run = true;
              description = "Ensure literate sources are tangled";
              enable = true;
              entry =
                let
                  make-srcs = pkgs.writeShellApplication {
                    name = "make-srcs";
                    runtimeInputs = with pkgs; [
                      coreutils
                      gnumake
                      noweb
                      perl
                    ];
                    text = "make srcs";
                  };
                in
                "${make-srcs}/bin/make-srcs";
            };
            nixpkgs-fmt.enable = true;
            ormolu = {
              enable = true;
              # TODO: create PR to add noCabal
              entry =
                let
                  inherit (config.pre-commit.settings.hooks.ormolu) settings;
                  extensions =
                    lib.escapeShellArgs (lib.concatMap (ext: [ "--ghc-opt" "-X${ext}" ]) settings.defaultExtensions);
                  noCabal = "--no-cabal";
                in
                  lib.mkForce "${pkgs.ormolu}/bin/ormolu --mode inplace ${extensions} ${noCabal}";
              settings.defaultExtensions = [
                "OverloadedStrings"
                "TemplateHaskell"
              ];
            };
          };
        };
      };
    };
}
