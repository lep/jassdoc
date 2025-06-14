{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
      mkdocs = pkgs: pkgs.haskellPackages.callPackage ./jassdoc.nix { };
      jass-db = pkgs:
        let fs = pkgs.lib.fileset;
        in pkgs.stdenv.mkDerivation {
          src = fs.toSource {
            root = ./.;
            fileset = fs.unions [
              ./GNUmakefile
              # perl scripts
              ./annotate
              ./mksrc
              ./mkmetadata
              ./lint
              # sql
              ./check-wrong-params.sql
              # jass files
              ./common.j
              ./common.ai
              ./Blizzard.j
              ./builtin-types.j
            ];
          };
          name = "jass.db";
          buildInputs = [
            pkgs.gnumake
            pkgs.perl
            pkgs.perlPackages.DBI
            pkgs.perlPackages.DBDSQLite
            pkgs.sqlite
            (mkdocs pkgs)
          ];
          buildPhase = ''
            MKDOCS=mkdocs make jass.db
          '';

          installPhase = ''
            install -Dt $out jass.db
          '';

          checkPhase = "make check";
          doCheck = true;
        };
    in {
      packages = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          mkdocs = mkdocs pkgs;
          jass-db = jass-db pkgs;
          default = jass-db pkgs;
        });

      devShells = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            inputsFrom = [ (jass-db pkgs) ];
            nativeBuildInputs = [ pkgs.cabal-install ];
          };
        });
    };
}
