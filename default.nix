{ mkDerivation, ansi-wl-pprint, base, hashable, stdenv, tasty
, tasty-golden, trifecta, unordered-containers
}:
mkDerivation {
  pname = "consubseq";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base hashable trifecta unordered-containers
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base hashable trifecta unordered-containers
  ];
  testHaskellDepends = [
    ansi-wl-pprint base hashable tasty tasty-golden
  ];
  license = stdenv.lib.licenses.publicDomain;
}
