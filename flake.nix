{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        hspkgs = pkgs.haskell.packages.ghc9101;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            hspkgs.ghc
            hspkgs.cabal-install
            hspkgs.haskell-language-server
            hspkgs.hspec-golden
            hspkgs.blaze-from-html
            nodejs
            eslint_d
            nodePackages.typescript-language-server
            hlint
            esbuild
            zlib
          ];
        };
      });
}
