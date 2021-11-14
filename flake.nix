{
  description = "Track the publish dates of your favorite comics";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, emacs-overlay, flake-utils, nixpkgs }:
    {
      overlay = nixpkgs.lib.composeManyExtensions (nixpkgs.lib.attrValues self.overlays);

      overlays = {
        haskellPackages = final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = hfinal: hprev: {
              ComiCal = hprev.callCabal2nix "ComiCal" self { };
            };
          };
        };
        myEmacs = nixpkgs.lib.composeExtensions emacs-overlay.overlay (final: prev: {
          myEmacs = prev.emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          };
        });
        pre-commit = final: prev: {
          gitAndTools = prev.gitAndTools // {
            pre-commit = prev.gitAndTools.pre-commit.overrideAttrs (oldAttrs: {
              propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [
                prev.cargo # NOTE: for nixpkgs-fmt hook
                prev.python3Packages.ruamel_yaml # NOTE: for check-yaml hook
              ];
            });
          };
        };
      };
    } // flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [ self.overlay ];
          inherit system;
        };
      in
      {
        apps.ComiCal = flake-utils.lib.mkApp {
          drv = pkgs.haskell.lib.justStaticExecutables self.defaultPackage.${system};
        };
        defaultApp = self.apps.${system}.ComiCal;
        defaultPackage = self.packages.${system}.ComiCal;
        devShell = pkgs.mkShell {
          FONTCONFIG_FILE = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.iosevka ];
          };

          buildInputs = with pkgs; [
            cabal-install
            ghcid
            gitAndTools.pre-commit
            haskell-language-server
            haskellPackages.ormolu
            haskellPackages.pointfree
            hlint
            myEmacs
            nixpkgs-fmt
            noweb
            (
              texlive.combine {
                inherit noweb;
                inherit (texlive) scheme-small
                  catchfile
                  datetime
                  fancyref
                  fmtcount
                  hardwrap
                  latexmk
                  mathpazo
                  titlesec
                  tufte-latex
                  xetex
                  ;
              }
            )
            which
          ] ++ self.defaultPackage.${system}.env.nativeBuildInputs;
        };
        packages = { inherit (pkgs.haskellPackages) ComiCal; };
      });
}
