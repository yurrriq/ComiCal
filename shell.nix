{ pkgs ? import ./nix }:
let
  project = import ./. { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs =
    project.env.nativeBuildInputs ++ (
      with pkgs; [
        cargo
        nix-prefetch-git
        niv
      ]
    ) ++ (
      with pkgs.haskellPackages; [
        cabal2nix
        cabal-install
        hindent
        hlint
        hpack
        pointfree
      ]
    ) ++ (
      with pkgs.python3Packages; [
        pre-commit
        yamllint
      ]
    );
}
