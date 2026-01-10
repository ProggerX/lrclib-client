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
        opts = {
          root = ./.;
          source-overrides = {}; # Put overrides here
        };
        pkg = op': hpkgs.developPackage (opts // op');
      in {
        packages.default = pkg {};
        devShells.default = pkg {
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
