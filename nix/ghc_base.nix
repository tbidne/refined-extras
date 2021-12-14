{ compilerVersion }:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/b0bf5f888d377dd2f36d90340df6dc9f035aaada.tar.gz") { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
