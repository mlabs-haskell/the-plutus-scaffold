{
  description = "Mlabs Plutus Template";

  nixConfig = {
    # extra-substituters = [ "https://cache.iog.io" ];
    # extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/v5.0.0;
    # haskell-nix.follows = "plutip/haskell-nix";
    tooling.url = github:mlabs-haskell/mlabs-tooling.nix;
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    # onchain plutarch
    # TODO: nixpkg follows?
    ply.url = github:mlabs-haskell/ply?ref=0.5.0;
    plutarch.url = "github:Plutonomicon/plutarch-plutus?ref=95e40b42a1190191d0a07e3e4e938b72e6f75268";
    psm.url = github:mlabs-haskell/plutus-simple-model;
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    haskell-nix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, cardano-transaction-lib, tooling, flake-utils, pre-commit-hooks, CHaP, ... }:
    # This seems preferable to a bunch of perSystem functions
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          # GENERAL

          # The overlays are only there for the offchain/frontend CTL outputs,
          # might make sense to have a different pkgs variant for the onchain
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              haskell-nix.overlay
              cardano-transaction-lib.overlays.purescript
              cardano-transaction-lib.overlays.runtime
              cardano-transaction-lib.overlays.spago
            ];
            inherit (haskell-nix) config;
          };

          # If we don't explicitly set this, tooling throws an error. Not sure why,
          # seems like it should default to this as far as I can understand the tooling
          # nix functions
          compiler-nix-name = "ghc925";

          pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix { });

          # This installs the pre-commit hooks (and runs them, I think? Maybe?)
          preCommitDevShell = pkgs.mkShell {
            name = "pre-commit-env";
            inherit (pre-commit-check) shellHook;
          };

          # This gives us a flake
          onchain-plutarch =
            let
              project = {
                inherit compiler-nix-name;
                src = ./onchain;
                name = "mlabs-plutus-template-project";
                extraHackage = [
                  "${inputs.ply}/ply-core"
                  "${inputs.ply}/ply-plutarch"
                  "${inputs.plutarch}"
                  "${inputs.plutarch}/plutarch-extra"
                  "${inputs.psm}/psm"
                  "${inputs.psm}/cardano-simple"
                ];

                modules = [
                  (_: {
                    packages = {
                      allComponent.doHoogle = true;
                      allComponent.doHaddock = true;
                    };
                  })
                ];
                shell = {
                  withHoogle = true;
                  exactDeps = true;
                  tools = {
                    cabal = { };
                    haskell-language-server = { };
                  };
                };
                inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
              };

              flakeModule = tooling.lib.mkHaskellFlakeModule1 { inherit project; };

            in
            tooling.lib.mkFlake { inherit self; } {
              imports = [ flakeModule ];
            };

          # Executables / Apps
          # If we used a perSystem approach, we could move these (after converting to a perSystem)
          # into the onchain-plutarch binding, but b/c we need the preCommitDevShell at the top level
          # it's easier to just define them separately
          packages =
            let
              script-exporter =
                let
                  exporter = onchain-plutarch.packages.${system}."mlabs-plutus-template-onchain:exe:exporter";
                in
                pkgs.runCommandLocal "script-exporter" { }
                  ''
                    ln -s ${exporter}/bin/exporter $out
                  '';

              exported-scripts =
                let
                  exporter = onchain-plutarch.packages.${system}."mlabs-plutus-template-onchain:exe:exporter";
                in
                pkgs.runCommand "exported-scripts" { }
                  ''
                    ${exporter}/bin/exporter $out
                  '';
            in
            {
              script-exporter = script-exporter;
              exported-scripts = exported-scripts;
            };


          # You can't add this directly in the project above b/c the mlabs-tooling functions
          # error out w/ conflicting shellHooks. Have to override the attrs after mlabs-tooling
          # has done its thing
          shellHook =
            ''
              export LC_CTYPE=C.UTF-8
              export LC_ALL=C.UTF-8
              export LANG=C.UTF-8
              ${pre-commit-check.shellHook}
            '';

          # OFFCHAIN / Testnet, Cardano, ...

          offchain =
            pkgs.purescriptProject {
              inherit pkgs;
              projectName = "mlabs-plutus-template-project";
              strictComp = false; # TODO: this should be eventually removed
              src = ./offchain;
              shell = {
                withRuntime = true;
                packageLockOnly = true;
                packages = with pkgs; [
                  # bashInteractive
                  # docker
                  fd
                  nodePackages.eslint
                  nodePackages.prettier
                  # ogmios
                  # ogmios-datum-cache
                  # plutip-server
                  # postgresql
                  # arion
                  # fd
                  # nixpkgs-fmt
                  # nodePackages.eslint
                  # nodePackages.prettier
                ];
                shellHook =
                  ''
                    export LC_CTYPE=C.UTF-8
                    export LC_ALL=C.UTF-8
                    export LANG=C.UTF-8
                    ${pre-commit-check.shellHook}
                  '';
              };
            };
        in
        rec {
          inherit pkgs;

          inherit offchain;

          inherit packages;

          inherit pre-commit-check;

          onchain = onchain-plutarch;
          pcc = pre-commit-check;

          checks = { inherit pre-commit-check; }
            //
            {
              mlabs-plutus-template = offchain.runPlutipTest { testMain = "Test"; };
            };

          devShells = {
            onchain = onchain.devShells.${system}.default.overrideAttrs (finalAttrs: previousAttrs: {
              shellHook = shellHook;
            });
            offchain = offchain.devShell;
            preCommit = preCommitDevShell;
            # This installs the pre-commit hooks, i.e.:
            #  - Generates a pre-commit-config.yaml
            #  - Modifies .git/hooks/pre-commit
            default = preCommitDevShell;
          };

          apps =
            onchain.apps.${system} // {
              docs = self.offchain.launchSearchablePursDocs { };
              ctl-docs = cardano-transaction-lib.apps.${system}.docs;
              script-exporter = {
                # nix run .#script-exporter -- onchain-scripts
                type = "app";
                program = packages.script-exporter.outPath;
              };
              ctl-runtime = cardano-transaction-lib.apps.${system}.ctl-runtime;
            };

        }
      );
}
