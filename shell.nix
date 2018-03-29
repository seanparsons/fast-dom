{ compiler ? "ghcjsHEAD" }:

let
  release = (import ./release.nix {inherit compiler;});
in release.pkgs.stdenv.lib.overrideDerivation release.fast-dom.env (oldAttrs: rec {
  nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
    release.cabal
    release.pkgs.awscli
    release.pkgs.haskellPackages.cabal2nix
    release.pkgs.haskellPackages.steeloverseer
    release.haskellPackages.hoogle
    release.pkgs.closurecompiler
  ];
})