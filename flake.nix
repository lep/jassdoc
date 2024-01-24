{
  inputs = {
    # nixpkgs.url = "github:NixOs/nixpkgs";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems }:
    let eachSystem = nixpkgs.lib.genAttrs (import systems);
    in {
      packages = eachSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };

          ghc = pkgs.haskellPackages.ghcWithPackages (ps: [
            ps.megaparsec
            ps.optparse-applicative
            ps.bytestring
            ps.aeson
          ]);

          mkdocs = pkgs.stdenv.mkDerivation {
            name = "mkdocs";
            src = self;
            buildPhase = ''
              ${ghc}/bin/ghc mkdocs
            '';

            installPhase = ''
              install -Dt $out/bin mkdocs
            '';

          };

          jassdoc = pkgs.stdenv.mkDerivation {
            name = "jassdoc";
            src = self;
            buildInputs = [ pkgs.gnumake pkgs.perl pkgs.sqlite ];

            buildPhase = ''
              MKDOCS=${mkdocs}/bin/mkdocs make jass.db
            '';

            installPhase = ''
              install -Dt $out jass.db
            '';
          };

        in {
          default = jassdoc;
          jassdoc = jassdoc;
          mkdocs = mkdocs;
        });
    };
}
