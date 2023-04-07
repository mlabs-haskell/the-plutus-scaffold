{
  description = "Mlabs Plutus Template";

  nixConfig = {
    # extra-substituters = [ "https://cache.iog.io" ];
    # extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    plutip.url = "github:mlabs-haskell/plutip/89cf822c213f6a4278a88c8a8bb982696c649e76";
    # plutip.url = github:mlabs-haskell/plutip/8364c43ac6bc9ea140412af9a23c691adf67a18b;
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/v5.0.0;
    haskell-nix.follows = "plutip/haskell-nix";
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
    /* NOTE (for Karol): We can't modify the shellHook for the onchain-plutarch
                         project if we use tooling.lib.mkFlake (I'm not sure why
                         but we get an error that afaict cannot be resolved).
                         Plutip's haskell-nix is out of date, and we can't use
                         ghc925 w/ the cabalProject' function we get from there.

                         This is the only way I could think of to make things work.
    */
    haskell-nix2.url = "github:input-output-hk/haskell.nix";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, haskell-nix2, plutip, cardano-transaction-lib, tooling, flake-utils, pre-commit-hooks, CHaP, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          # GENERAL

          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              haskell-nix2.overlay
              cardano-transaction-lib.overlays.purescript
              cardano-transaction-lib.overlays.runtime
              cardano-transaction-lib.overlays.spago
            ];
            inherit (haskell-nix2) config;
          };

          compiler-nix-name = "ghc925";

          pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix { });

          preCommitDevShell = pkgs.mkShell {
            name = "pre-commit-env";
            inherit (pre-commit-check) shellHook;
          };

          onchain-project = {
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

                shellHook = ''
                  export LC_CTYPE=C.UTF-8
                  export LC_ALL=C.UTF-8
                  export LANG=C.UTF-8
                  ${pre-commit-check.shellHook}
                '';
                };
                inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
              };

          flakeModule = tooling.lib.mkHaskellFlakeModule1 {project = onchain-project;};

          onchain-plutarch =  tooling.lib.mkFlake {inherit self;} {
              imports = [ flakeModule ];
          };

          onchain-plutarch''' = pkgs.haskell-nix.cabalProject' [
            tooling.lib.mkHackageMod
            onchain-project ];

          # ONCHAIN / Plutarch
          onchain-plutarch' = tooling.lib.mkFlake { inherit self; }
            {
              imports = [
                (tooling.lib.mkHaskellFlakeModule1 {
                  project.src = ./onchain;
                  # project.compiler-nix-name = "ghc8107";
                  project.extraHackage = [
                    "${inputs.ply}/ply-core"
                    "${inputs.ply}/ply-plutarch"
                    "${inputs.plutarch}"
                    "${inputs.plutarch}/plutarch-extra"
                    "${inputs.psm}/psm"
                    "${inputs.psm}/cardano-simple"
                  ];
                  project.shell.shellHook = ''
                    export LC_CTYPE=C.UTF-8
                    export LC_ALL=C.UTF-8
                    export LANG=C.UTF-8
                    ${pre-commit-check.shellHook}
                  '';
                })
              ];
            };

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

          inherit flakeModule;

          onchain = onchain-plutarch;
          pcc = pre-commit-check;

          packages =
            let
              script-exporter =
                let
                  exporter = onchain.packages.${system}."mlabs-plutus-template-onchain:exe:exporter";
                in
                  pkgs.runCommandLocal "script-exporter" {  }
                    ''
                      ln -s ${exporter}/bin/exporter $out
                    '';

              exported-scripts =
                let
                  exporter = onchain.packages.${system}."mlabs-plutus-template-onchain:exe:exporter";
                in
                  pkgs.runCommand "exported-scripts" {  }
                    ''
                      ${exporter}/bin/exporter $out
                    '';
            in
              {
            script-exporter = script-exporter;
            exported-scripts = exported-scripts;
              };

          checks = { inherit pre-commit-check; }
            //
            {
              mlabs-plutus-template = self.offchain.${system}.runPlutipTest { testMain = "Test"; };
            };

          devShells =  {
            onchain = onchain.devShells.${system}.default.overrideAttrs (finalAttrs: previousAttrs: {
              shellHook = shellHook;
            });
            offchain = self.offchain.${system}.devShell;
            # This installs the pre-commit hooks, i.e.:
            #  - Generates a pre-commit-config.yaml
            #  - Modifies .git/hooks/pre-commit
            default = preCommitDevShell;
          };

          apps =
            onchain.apps.${system} // {
              docs = self.offchain.${system}.launchSearchablePursDocs { };
              ctl-docs = cardano-transaction-lib.apps.${system}.docs;
              script-exporter = {
                # nix run .#script-exporter -- onchain-scripts
                type = "app";
                program = self.packages.${system}.script-exporter.outPath;
              };
              ctl-runtime = cardano-transaction-lib.apps.${system}.ctl-runtime;
            };

        }
      );
    }
