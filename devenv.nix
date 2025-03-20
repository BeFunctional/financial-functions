{ pkgs, lib, config, inputs, ... }:

{

  # https://devenv.sh/packages/
  packages = [ pkgs.llvm pkgs.ormolu pkgs.nixfmt pkgs.haskellPackages.hlint ];

  # https://devenv.sh/languages/
  # languages.rust.enable = true;

  # https://devenv.sh/processes/
  # processes.cargo-watch.exec = "cargo-watch";

  # https://devenv.sh/services/
  # services.postgres.enable = true;

  # https://devenv.sh/scripts/

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc964;
  };

  pre-commit.hooks = {
    # lint shell scripts
    shellcheck.enable = false;
    markdownlint.enable = false;
    # lint nix
    nixfmt.enable = true;
    deadnix.enable = false;
    nil.enable = false;
    # format haskell
    ormolu.enable = true;
    cabal-fmt.enable = true;
    # lint haskell
    hlint.enable = true;
  };

}
