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
        packages = { inherit (pkgs.haskellPackages) ComiCal; };
        defaultPackage = self.packages.${system}.ComiCal;
        devShell = pkgs.mkShell {
          FONTCONFIG_FILE = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.iosevka ];
          };

          buildInputs = with pkgs; [
            cabal-install
            haskellPackages.pointfree
            myEmacs
            ghcid
            haskell-language-server
            rnix-lsp
            noweb
            pythonPackages.pygments
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
                  # fancyref
                  latexmk
                  minted
                  todonotes
                  xetex
                  ;
              }
            )
            pythonPackages.pywatchman
            semver-tool
            nixpkgs-fmt
            pre-commit
            haskellPackages.ormolu
            hlint
            shellcheck
            which
          ] ++ self.defaultPackage.${system}.env.nativeBuildInputs;
        };
      });
}
