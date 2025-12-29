{
  outputs = { nixpkgs, flake-utils, ... }:
  flake-utils.lib.eachDefaultSystem(system:
  let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [(final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = hself: hsuper: {
            MIP = prev.haskell.lib.overrideCabal hsuper.MIP (oldAttrs: {
              doCheck = false;
              broken = false;
            });
          };
        };
      })];
    };
  in {
    devShells.default = (pkgs.haskellPackages.extend
      (pkgs.haskell.lib.compose.packageSourceOverrides {
        aoc = ./.;
      })
    ).shellFor {
      packages = p: [ p.aoc ];
      withHoogle = true;
      buildInputs = [ pkgs.xmlstarlet pkgs.cbc ];
    };
  });
}
