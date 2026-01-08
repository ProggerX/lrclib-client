{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = {
    flake-parts,
    nixpkgs,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.platforms.unix;
      perSystem = {pkgs, ...}: let
        hpkgs = pkgs.haskellPackages;
        pkg = hpkgs.callCabal2nix "light" ./. {};
      in {
        packages.default = pkg;
        devShells.default = hpkgs.developPackage {
          root = ./.;
          returnShellEnv = true;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs; [
              cabal-install
              haskell-language-server
            ]);
        };
      };
    };
}
