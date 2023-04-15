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
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
  };

  outputs = { self, emacs-overlay, flake-utils, nixpkgs }:
    {
      overlays = {
        default = nixpkgs.lib.composeManyExtensions
          (nixpkgs.lib.attrValues
            (nixpkgs.lib.filterAttrs (n: _: n != "default") self.overlays));

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
          overlays = [ self.overlays.default ];
          inherit system;
        };
      in
      {
        apps = {
          ComiCal = flake-utils.lib.mkApp {
            drv = pkgs.haskell.lib.justStaticExecutables self.packages.${system}.ComiCal;
          };
          default = self.apps.${system}.ComiCal;
        };
        packages = {
          inherit (pkgs.haskellPackages) ComiCal;
          default = self.packages.${system}.ComiCal;
        };
        devShells.default = pkgs.mkShell {
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
                  # fancyref
                  latexmk
                  minted
                  todonotes
                  xetex
                  ;
              }
            )
            python3Packages.pywatchman
            semver-tool
            nixpkgs-fmt
            pre-commit
            haskellPackages.ormolu
            hlint
            shellcheck
            which
          ] ++ self.packages.${system}.ComiCal.env.nativeBuildInputs;
        };
      });
}
