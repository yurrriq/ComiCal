{ pkgs ? import ./nix/nixpkgs.nix {} }:

pkgs.haskellPackages.callPackage ./default.nix {}
