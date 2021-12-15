{
  description = "refined-extras flake";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs =
    { flake-utils
    , nixpkgs
    , self
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc8107";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "refined-extras";
          root = ./.;
          # Doctests are refusing to work, again. This time the complaint is:
          #
          # src/Refined/Utils.hs:12:1: error:
          #     Ambiguous module name ‘Data.These’:
          #       it was found in multiple packages: these-1.1.1.1 these-skinny-0.7.4
          #
          # which seems ridiculous, because 'these' is not mentioned in the
          # cabal file...previously this issue was handled by passing
          # --write-ghc-environment-files=always to cabal build, but I
          # cannot figure out how to pass arbitrary flags to cabal.
          # cabal2nixOptions does not take arbitrary cabal flags,
          # and overriding the buildFlags and/or configureFlags
          # via overrideCabal has also been unsuccessful (errors mention
          # the flag not existing).
          #
          # Thus, we are disabling tests until we can figure this out.
          cabal2nixOptions = "--no-check";
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
              cabal-fmt
              cabal-install
              cabal-plan
              haskell-language-server
              hlint
              ghcid
              implicit-hie
              ormolu
              pkgs.nixpkgs-fmt
              pkgs.zlib
            ]);
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
