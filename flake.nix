{
  description = "Mlabs Plutus Template";

  nixConfig = {
    allow-import-from-derivation = "true";
  };

  inputs = {
    cardano-transaction-lib.url = "github:Plutonomicon/cardano-transaction-lib/v5.0.0";
    mlabs-tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    flake-parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    ply.url = "github:mlabs-haskell/ply?ref=0.5.0";
    plutarch.url = "github:Plutonomicon/plutarch-plutus?ref=95e40b42a1190191d0a07e3e4e938b72e6f75268";
    psm.url = "github:mlabs-haskell/plutus-simple-model";
    nixpkgs-oldctl.follows = "cardano-transaction-lib/nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, cardano-transaction-lib, mlabs-tooling, flake-parts, nixpkgs-oldctl, ... }:
    let
      # We leave it to just linux to be able to run `nix flake check` on linux, 
      # see bug https://github.com/NixOS/nix/issues/4265
      systems = [ "x86_64-linux" ];
      # systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      project-name = "plutus-scaffold";

      # ONCHAIN / Plutarch
      onchain = mlabs-tooling.lib.mkFlake { inherit self; }
        {
          imports = [
            (mlabs-tooling.lib.mkHaskellFlakeModule1 {
              project.src = (builtins.path {
                path = ./onchain;
                name = "${project-name}-src";
              });
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

          inherit systems;

          perSystem = { self', pkgs, ... }:
            let
              script-exporter =
                let
                  exporter = self'.packages."mlabs-plutus-template-onchain:exe:exporter";
                in
                pkgs.runCommandLocal "script-exporter" { }
                  ''
                    ln -s ${exporter}/bin/exporter $out
                  '';

              exported-scripts =
                let
                  exporter = self'.packages."mlabs-plutus-template-onchain:exe:exporter";
                in
                pkgs.runCommand "exported-scripts" { }
                  ''
                    ${exporter}/bin/exporter $out
                  '';
            in
            {
              packages = {
                inherit script-exporter exported-scripts;
              };
            };
        };

    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        (import ./pre-commit.nix)
      ];
      inherit systems;
      perSystem = { system, config, ... }:
        let

          # ctl overlays
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              cardano-transaction-lib.overlays.purescript
              cardano-transaction-lib.overlays.runtime
              cardano-transaction-lib.overlays.spago
            ];
          };

          offchain =
            let
              projectName = "${project-name}-offchain";
              offchain-dir = (builtins.path {
                path = ./offchain;
                name = "${projectName}-src";
              });
              compiled-scripts-dir = onchain.packages.${system}.exported-scripts;
            in
            pkgs.purescriptProject rec {
              inherit pkgs;
              inherit projectName;
              # If warnings generated from project source files will trigger a build error
              strictComp = false;
              # We extend the offchain source with compiled-scripts to run tests with offchain.runPlutipTests
              src = pkgs.runCommand "${projectName}-src-w-scripts" { } ''
                mkdir $out
                cp -r ${offchain-dir} $out/offchain
                cp -r ${compiled-scripts-dir} $out/compiled-scripts
              '';
              packageJson = "${src}/offchain/package.json";
              packageLock = "${src}/offchain/package-lock.json";
              spagoPackages = "${src}/offchain/spago-packages.nix";
              shell = {
                withRuntime = true;
                packageLockOnly = true;
                packages = with pkgs; [
                  fd
                  nodePackages.eslint
                  nodePackages.prettier
                ];
              };
            };

          # Bundles with `spago bundle-module`, sharing the built project with the offchain purescriptProject
          bundlePsModule =
            { main
            , bundledModuleName ? "Offchain.js"
            , ...
            }:
            let
              name = "${project-name}-ps-${main}-module-bundled";
              inherit bundledModuleName;
              # project's source + spago output/ 
              project = offchain.compiled;
            in
            pkgs.runCommand name
              {
                nativeBuildInputs = [
                  project
                  offchain.purs
                  pkgs.easy-ps.spago
                ];
              }
              ''
                export HOME="$TMP"
                cp -r ${project}/* .
                chmod -R +rwx .
                spago bundle-module --no-install --no-build -m "${main}" \
                  --to ${bundledModuleName}
                mkdir $out
                cp ${bundledModuleName} $out
              '';

          # Used to add pre-commit packages and shell hook to the other project shells
          mergeShells = devshell-1: devshell-2: pkgs.mkShell {
            packages = [ ];

            inputsFrom = [ devshell-1 devshell-2 ];

            shellHook = devshell-1.shellHook + devshell-2.shellHook;
          };

          # haskell development shell, with pre-commit shellhook
          onchain-devshell = mergeShells onchain.devShells.${system}.default config.pre-commit.devShell;
          # purescript development shell, with pre-commit shellhook
          # offchain-devshell = appendShellHook offchain.devShell config.pre-commit.installationScript;
          offchain-devshell = mergeShells offchain.devShell config.pre-commit.devShell;

          # older, ctl's nixpkgs, quick fix of ctl-runtime, broken with pkgs update
          pkgs-oldctl = import nixpkgs-oldctl {
            inherit system;
            overlays = [
              cardano-transaction-lib.overlays.purescript
              cardano-transaction-lib.overlays.runtime
              cardano-transaction-lib.overlays.spago
            ];
          };
        in
        {

          packages = onchain.packages.${system}
            // {
            bundle-offchain-api = bundlePsModule { main = "Api"; };
          };

          checks = { }
            # ;
            # onchain.checks.${system}
            //
            {
              psm-tests = onchain.checks.${system}."mlabs-plutus-template-onchain:test:psm-test";
              plutipTests = offchain.runPlutipTest { testMain = "Test.Scaffold.Main"; };
            };

          devShells = {
            frontend = pkgs.mkShell {
              packages = [ pkgs.nodejs ];
            };
            onchain = onchain-devshell;
            offchain = offchain-devshell;
          };

          apps =
            {
              docs = offchain.launchSearchablePursDocs { };
              ctl-docs = cardano-transaction-lib.apps.${system}.docs;
              script-exporter = {
                # nix run .#script-exporter -- onchain-scripts
                type = "app";
                program = self.packages.${system}.script-exporter.outPath;
              };
              ctl-runtime = pkgs-oldctl.launchCtlRuntime { };
              ctl-blockfrost-runtime = pkgs-oldctl.launchCtlRuntime { blockfrost.enable = true; };
            };
        };
      flake = {
        # On CI, build only on available systems, to avoid errors about systems without agents.
        herculesCI.ciSystems = [ "x86_64-linux" ];

        # Used by `nix flake init -t <flake>`
        templates.default = {
          path = ./.;
          description = "Mlabs Plutus project template";
          welcomeText = ''
            Welcome in the Plutus scaffold!
              
            To enter the Nix environment, run `nix develop .#onchain` or `nix develop .#offchain` respectively.
            Frontend is managed by npm, see the `frontend/package.json` scripts field.

            For offchain consult [ctl docs](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc).
          ''; # TODO: revisit when we have docs
        };
      };
    };
}
