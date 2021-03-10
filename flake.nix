{
  description = "Track the publish dates of your favorite comics";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
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
      };
    in
    { inherit overlay; } // flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [ overlay ];
          inherit system;
        };
      in
      pkgs.lib.fix (this: {
        packages.ComiCal = pkgs.haskellPackages.ComiCal;

        apps.ComiCal = flake-utils.lib.mkApp {
          drv = pkgs.haskell.lib.justStaticExecutables this.defaultPackage;
        };

        defaultApp = this.apps.ComiCal;

        defaultPackage = this.packages.ComiCal;

        devShell = pkgs.mkShell {
          buildInputs =
            this.defaultPackage.env.nativeBuildInputs ++ (
              with pkgs; [
                gitAndTools.pre-commit
                python3Packages.yamllint
                yq
              ]
            ) ++ (
              with pkgs.haskellPackages; [
                cabal-install
                hlint
                hpack
                ormolu
                pointfree
              ]
            );
        };
      }));
}
