{ nixpkgs ? <nixpkgs> }:
let
  pkgs = import nixpkgs {};
in
pkgs.callPackage ./web-client.nix {}
