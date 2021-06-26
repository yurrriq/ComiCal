{
  description = "Track the publish dates of your favorite comics";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, emacs-overlay, flake-utils, nixpkgs }:
    {
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = hfinal: hprev: {
            ComiCal = hfinal.callCabal2nix "ComiCal" self { };
            req = hprev.callHackageDirect
              {
                pkg = "req";
                ver = "3.9.0";
                sha256 = "sha256-2NykJlUKhRAHa1lxDj4xWxruSwHFU98xYNNiVSZAKes=";
              }
              { };
          };
        };

        myEmacs = prev.emacsWithPackagesFromUsePackage {
          alwaysEnsure = true;
          config = ./emacs.el;
        };
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          overlays = [
            emacs-overlay.overlay
            self.overlay
          ];
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
          buildInputs = with pkgs; [
            cabal-install
            ghcid
            gitAndTools.pre-commit
            haskell-language-server
            haskellPackages.ormolu
            haskellPackages.pointfree
            hlint
            myEmacs
            python3Packages.yamllint
          ] ++ self.defaultPackage.${system}.env.nativeBuildInputs;
        };

        packages = { inherit (pkgs.haskellPackages) ComiCal; };
    });
}
