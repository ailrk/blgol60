{ mkDerivation, base, bytestring, containers, deepseq, generic-lens
, hashable, hspec, lens, lib, megaparsec, mtl, parser-combinators
, pretty-simple, relude, string-interpolate, text, transformers
, unliftio, unordered-containers
}:
mkDerivation {
  pname = "blgol60";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring containers deepseq generic-lens hashable lens
    megaparsec mtl parser-combinators pretty-simple relude
    string-interpolate text transformers unliftio unordered-containers
  ];
  executableHaskellDepends = [
    base bytestring containers deepseq generic-lens hashable lens
    megaparsec mtl parser-combinators pretty-simple relude
    string-interpolate text transformers unliftio unordered-containers
  ];
  testHaskellDepends = [ base hspec pretty-simple text unliftio ];
  license = lib.licenses.mit;
  mainProgram = "blgol60";
}
