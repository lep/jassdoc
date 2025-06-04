{ mkDerivation, aeson, base, bytestring, lib, megaparsec, mtl
, optparse-applicative, parser-combinators, text
}:
mkDerivation {
  pname = "jassdoc";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring megaparsec mtl optparse-applicative
    parser-combinators text
  ];
  license = "unknown";
  mainProgram = "mkdocs";
}
