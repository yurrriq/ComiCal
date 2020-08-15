{ pkgs ? import ./nix }:
let
  src = pkgs.nix-gitignore.gitignoreRecursiveSource [ ".git/" ] ./.;
in
pkgs.haskellPackages.callCabal2nix "ComiCal" src {}
