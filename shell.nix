{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-wl-pprint, base, hashable, stdenv, tasty
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
