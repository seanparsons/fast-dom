{ compiler ? "ghcjsHEAD" }:

(import ./release.nix {inherit compiler;}).fast-dom