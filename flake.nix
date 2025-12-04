{
  outputs = { nixpkgs, flake-utils, ... }:
  flake-utils.lib.eachDefaultSystem(system:
  let
    pkgs = import nixpkgs { inherit system; };
  in {
    devShells.default = (pkgs.haskellPackages.extend
      (pkgs.haskell.lib.compose.packageSourceOverrides {
        aoc = ./.;
      })
    ).shellFor {
      packages = p: [ p.aoc ];
      withHoogle = true;
      buildInputs = [ pkgs.xmlstarlet ];
    };
  });
}
