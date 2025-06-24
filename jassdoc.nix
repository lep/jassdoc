{ mkDerivation, aeson, base, bytestring, lib, megaparsec, mtl
, optparse-applicative, parser-combinators, text, file-embed }:
let fs = lib.fileset;
in mkDerivation {
  pname = "jassdoc";
  version = "1.0.0";
  src = fs.toSource {
    root = ./.;
    fileset = fs.unions [ ./jassdoc.cabal ./src ];
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    bytestring
    megaparsec
    mtl
    optparse-applicative
    parser-combinators
    text
    file-embed
  ];
  license = "unknown";
  mainProgram = "mkdocs";
}
