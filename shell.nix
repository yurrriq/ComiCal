{ pkgs ? import ./nix/nixpkgs.nix {} }:

let
  project = import ./release.nix { inherit pkgs; };
in

pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs;
}
