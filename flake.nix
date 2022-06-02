{
  description = "Track the publish dates of your favorite comics";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
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
            haskell-language-server
            haskellPackages.ormolu
            haskellPackages.pointfree
            hlint
            myEmacs
            nixpkgs-fmt
            noweb
            pre-commit
            pythonPackages.pygments
            pythonPackages.pywatchman
            rnix-lsp
            semver-tool
            (
              texlive.combine {
                inherit noweb;
                inherit (texlive) scheme-small
                  catchfile
                  datetime
                  dirtytalk
                  fancyref
                  fmtcount
                  framed
                  fvextra
                  hardwrap
                  latexmk
                  mathpazo
                  minted
                  titlesec
                  todonotes
                  tufte-latex
                  xetex
                  xstring
                  ;
              }
            )
            which
          ] ++ self.defaultPackage.${system}.env.nativeBuildInputs;
        };
        packages = { inherit (pkgs.haskellPackages) ComiCal; };
      });
}
