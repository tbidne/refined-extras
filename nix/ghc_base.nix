{ compilerVersion }:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/5bb20f9dc70e9ee16e21cc404b6508654931ce41.tar.gz") { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
