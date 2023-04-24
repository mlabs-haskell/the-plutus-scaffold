{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
  ];
  perSystem = { config, ... }:
    {
      devShells.dev-pre-commit = config.pre-commit.devShell;
      devShells.default = config.pre-commit.devShell;

      pre-commit = {
        settings = {

          excludes = [
            "spago-packages.nix"
            "offchain/src/Scripts\.[js|purs]"
            "compiled-scripts/"
          ];

          hooks = {
            # nix
            nixpkgs-fmt.enable = true;
            deadnix.enable = true;
            # statix.enable = true;

            # haskell
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;

            shellcheck.enable = true;
            markdownlint.enable = true;
            dhall-format.enable = true;

            # purescript
            purty.enable = true;
            # js, ts, almost everything if enabled
            prettier.enable = true;
            prettier.types_or = [ "javascript" "ts" "json" ];
          };

          settings.ormolu.cabalDefaultExtensions = true;
        };
      };
    };
}
