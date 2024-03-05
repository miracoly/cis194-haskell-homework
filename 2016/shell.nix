{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    haskellPackages.cabal-install
    zlib.dev
  ];
}
