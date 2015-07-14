{ mkDerivation, base, cmdargs, parallel-io, shelly, split, stdenv
, system-filepath, text
}:
mkDerivation {
  pname = "rehoo";
  version = "0.3.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base cmdargs parallel-io shelly split system-filepath text
  ];
  homepage = "https://github.com/jwiegley/rehoo";
  description = "Rebuild default.hoo from many .hoo files in the current directory";
  license = stdenv.lib.licenses.bsd3;
}
