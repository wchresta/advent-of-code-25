{
  outputs = { nixpkgs, flake-utils, ... }:
  flake-utils.lib.eachDefaultSystem(system:
  let
    pkgs = import nixpkgs { inherit system; };
  in {
    devShells.default = pkgs.haskellPackages.shellFor {
      packages = p: [ p.criterion ];
      withHoogle = true;
      buildInputs = [ pkgs.xmlstarlet ];
    };
  });
}
