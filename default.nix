{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./graphql-api.nix {}
