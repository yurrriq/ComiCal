self: super: {

  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      ComiCal = hself.callPackage ../. {};
      req = hsuper.callPackage ./req.nix {};
    };
  };

  inherit (import (import ./sources.nix).niv { pkgs = super; }) niv;

}
