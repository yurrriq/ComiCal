define build_expr
with import <nixpkgs> {}; haskellPackages.callPackage ./. {}
endef


.PHONY: build
build: default.nix $(wildcard src/**.hs) $(wildcard app/**.hs)
	nix-build -E '${build_expr}'


default.nix: package.yaml
	cabal2nix . --maintainer yurrriq --hpack >$@
