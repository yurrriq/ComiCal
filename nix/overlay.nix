self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      req = hsuper.callPackage ./req.nix {};
    };
  };
}
