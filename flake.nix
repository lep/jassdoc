{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
      jass-files = pkgs:
        let fs = pkgs.lib.fileset;
        in pkgs.stdenv.mkDerivation {
          name = "jass-files";
          src = fs.toSource {
            root = ./.;
            fileset = fs.unions [
              ./annotate
              ./src/schema.sql
              ./common.j
              ./common.ai
              ./Blizzard.j
            ];
          };

          buildInputs = [
            pkgs.perl
            pkgs.perlPackages.DBI
            pkgs.perlPackages.DBDSQLite
            pkgs.sqlite
          ];

          buildPhase = ''
            sqlite3 empty.db < src/schema.sql
            perl annotate --db empty.db common.j common.ai Blizzard.j
          '';

          installPhase = ''
            mkdir -p $out
            install common.j-out $out/common.j
            install common.ai-out $out/common.ai
            install Blizzard.j-out $out/Blizzard.j
          '';
          
        };
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
              ./check-revision
              ./check-wrong-params
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
            GITREV=${if (self ? rev) then self.rev else "nix-dirty"} MKDOCS=mkdocs make jass.db
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
          jass-files = jass-files pkgs;
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
