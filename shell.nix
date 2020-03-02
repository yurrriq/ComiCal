{ pkgs ? import ./nix/nixpkgs.nix {} }:

let
  project = import ./release.nix { inherit pkgs; };
in

pkgs.mkShell {
  buildInputs =
    project.env.nativeBuildInputs ++ (
      with pkgs; [
        nix-prefetch-git
        niv
      ]
    ) ++ (
      with pkgs.haskellPackages; [
        cabal2nix
        cabal-install
        hindent
        hpack
        pointfree
      ]
    );
}
