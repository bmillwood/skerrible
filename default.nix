{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc96" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./skerrible.nix { }
