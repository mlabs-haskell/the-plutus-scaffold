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
    ply.url = github:mlabs-haskell/ply?ref=0.4.0;
    plutarch.url = "github:Plutonomicon/plutarch-plutus?ref=95e40b42a1190191d0a07e3e4e938b72e6f75268";
    psm.url = github:mlabs-haskell/plutus-simple-model;

  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutip, cardano-transaction-lib, tooling, flake-utils, pre-commit-hooks, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          # GENERAL
          # supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
          # supportedSystems = [ "x86_64-linux" ];

          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              haskell-nix.overlay # TODO: can actually remove?
              cardano-transaction-lib.overlays.purescript
              cardano-transaction-lib.overlays.runtime
              cardano-transaction-lib.overlays.spago
            ];
            inherit (haskell-nix) config;
          };

          pre-commit-check = pre-commit-hooks.lib.${system}.run (import ./pre-commit-check.nix { });

          preCommitDevShell = pkgs.mkShell {
            name = "pre-commit-env";
            inherit (pre-commit-check) shellHook;
          };

          # ONCHAIN / Plutarch
          onchain-plutarch = tooling.lib.mkFlake { inherit self; }
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
                })
              ];
            };

          # offchain


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
                    '';
                };
          };
        in
        rec {
          inherit pkgs;

          inherit offchain;

          onchain = onchain-plutarch;

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
            onchain = onchain-plutarch.devShells.${system}.default;
            offchain = self.offchain.${system}.devShell;
            # to make nix shut up
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
