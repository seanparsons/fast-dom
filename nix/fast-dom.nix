{ mkDerivation, base, ghcjs-dom, stdenv }:
mkDerivation {
  pname = "fast-dom";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ghcjs-dom ];
  license = stdenv.lib.licenses.bsd3;
}
