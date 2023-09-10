{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, }:
    flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };
          packageName = "jassdoc";
          mkdocs = pkgs.haskellPackages.callCabal2nix packageName self {
            # Dependency overrides go here
          };
          buildInputs = [ pkgs.gnumake pkgs.perl pkgs.sqlite mkdocs ];
        in rec {
          packages = {
            ${packageName} = pkgs.stdenv.mkDerivation {
              name = "jassdoc";
              src = self;
              inherit buildInputs;

              buildPhase = "MKDOCS=${mkdocs}/bin/mkdocs make jass.db";
              installPhase = ''
                install -Dt $out jass.db
              '';
            };
          };

          defaultPackage = packages.${packageName};

          devShell = pkgs.mkShell {
            buildInputs = buildInputs ++ [ pkgs.cabal-install ];
            inputsFrom = builtins.attrValues self.packages.${system};
            packages = [ pkgs.perlPackages.DBI pkgs.perlPackages.DBDSQLite ];
          };
        });
}
