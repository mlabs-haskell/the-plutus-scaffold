{ inputs, lib, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
  ];
  perSystem = { pkgs, system, inputs', config, ... }:
    {
      devShells.dev-pre-commit = config.pre-commit.devShell;
      devShells.default = config.pre-commit.devShell;

      pre-commit = {
        settings = {

          excludes = [
            "spago-packages.nix"
            "offchain/src/Scripts\.[js|purs]"
          ];

          hooks = {
            # nix
            nixpkgs-fmt.enable = true;
            deadnix.enable = true;
            statix.enable = true;

            # haskell
            cabal-fmt.enable = true;
            # fourmolu.enable = true;
            # hlint.enable = true;

            shellcheck.enable = true;
            markdownlint.enable = true;
            dhall-format.enable = true;

            # purescript
            purty.enable = true;
            # js, ts, almost everything if enabled
            prettier.enable = true;
            prettier.files = ".*\.js|.*\.ts|.*\.css";
          };

          # settings.ormolu.cabalDefaultExtensions = true;
        };
      };
    };
}
