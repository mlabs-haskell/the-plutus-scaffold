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
    dream2nix.url = "github:nix-community/dream2nix";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
  };

  outputs = inputs@{ self, nixpkgs, cardano-transaction-lib, mlabs-tooling, flake-parts, nixpkgs-oldctl, dream2nix, hercules-ci-effects, ... }:
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

          perSystem = { pkgs, config, ... }:
            let
              # TODO: rename mlabs-plutus-template to plutus-scaffold
              exporter = config.packages."mlabs-plutus-template-onchain:exe:exporter";

              script-exporter =
                pkgs.runCommandLocal "script-exporter" { }
                  ''
                    ln -s ${exporter}/bin/exporter $out
                  '';

              exported-scripts =
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
        hercules-ci-effects.flakeModule
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

          # Derviation producing directory like that:
          # mkDir {"offchain/src" : [some_derivation, other_derivation];
          #        "offchain": [./offchain]}
          # = 
          # /
          #   - offchain/
          #     - src/
          #       - some_derivation
          #       - other_derivation
          #     - ...
          # used instead of symlinkJoin, because https://github.com/nix-community/dream2nix/issues/520
          mkDir = name: dir: pkgs.runCommand name { } (
            ''
              mkdir res
            '' +
            (pkgs.lib.concatMapStrings
              (path:
                let
                  copyCmd =
                    if path == "" then
                      (drv: ''
                        cp -r ${drv}/* res
                      '')
                    else
                      (drv: ''
                        mkdir -p res/${path}
                        chmod +w res/${path}
                        cp -r ${drv}/* res/${path}
                      '');
                in
                pkgs.lib.concatMapStrings copyCmd (dir.${path})
              )
              (builtins.attrNames dir))
            + ''
              mkdir $out
              cp -r res/* $out
            ''
          );

          offchain =
            let
              # TAG: compiled-scripts
              projectName = "${project-name}-offchain";
              # Derivation producing:
              # / 
              #  - offchain
              #  - compiled-scripts
              # This should match your local development source tree
              offchain-src-w-scripts = mkDir "${projectName}-src-w-scripts" {
                "offchain" = [
                  (builtins.path {
                    path = ./offchain;
                    name = "${projectName}-src";
                  })
                ];
                "compiled-scripts" = [ onchain.packages.${system}.exported-scripts ];
              };
            in
            pkgs.purescriptProject rec {
              inherit pkgs;
              inherit projectName;
              # If warnings generated from project source files will trigger a build error
              strictComp = false;
              # We extend the offchain source with compiled-scripts to run tests with offchain.runPlutipTests
              src = offchain-src-w-scripts;
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
          bundlePsModule = main:
            let
              name = "${project-name}-bundle-${main}";
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
              # TAG: OFFCHAIN.JS
              ''
                export HOME="$TMP"
                cp -r ${project}/* .
                chmod -R +rwx .
                spago bundle-module --no-install --no-build -m "${main}" \
                  --to Offchain.js
                mkdir $out
                cp Offchain.js $out
              '';

          # TAG: SwitchContractParams
          # The different bundles used to differentiate by ContractParams
          offchain-api-bundles = {
            OffchainApiLocal = (bundlePsModule "OffchainApiLocal");
            OffchainApiDeployment = (bundlePsModule "OffchainApiDeployment");
          };

          # Derivation producing frontend source together with the offchain bundle in src:
          # This should match your local development source tree
          frontend-full-src = offchain-api-bundle: mkDir "frontend-full-src" {
            "" = [ ./frontend ];
            "src" = [ offchain-api-bundle ];
          };

          # frontend flake outputs, with offchain-api-bundle included into src
          frontend = offchain-api-bundle: dream2nix.lib.makeFlakeOutputs {
            systems = [ system ];
            config.projectRoot = ./frontend;
            # avoid symlinks in the source
            source = frontend-full-src offchain-api-bundle;
            projects.${project-name} = {
              name = "${project-name}";
              relPath = "";
              subsystem = "nodejs";
              translator = "package-lock";
              builder = "granular-nodejs";
            };
          };

          # Derivation producing frontend static website bundle.
          # Parametrized by the offchain bundle to include (to differentiate between ContractParams).
          frontend-bundle = offchain-api-bundle:
            let
              frontend' = frontend offchain-api-bundle;
              built-frontend = "${frontend'.packages.${system}.${project-name}}/lib/node_modules";
              # To produce the webpack bundle we need to include compiled-scripts in source
              frontend-full-src-w-scripts = mkDir "frontend-full-src-w-scripts" {
                # this below is almost like ./frontend, but with offchain bundle and node_modules
                "frontend" = [ "${built-frontend}/${project-name}" ];
                # TAG: compiled-scripts
                "compiled-scripts" = [ onchain.packages.${system}.exported-scripts ];
              };
              frontend-shell = frontend'.devShells.${system}.default;
            in
            pkgs.runCommand "frontend-bundle"
              {
                buildInputs = frontend-shell.buildInputs; # just nodejs
              }
              # We use the binaries and node_modules (included in the package) provided by frontend's nix
              ''
                export HOME="$TMP"
                export PATH="${built-frontend}/.bin:$PATH"
                cp -r ${frontend-full-src-w-scripts}/* .
                chmod -R +w frontend
                cd frontend
                npm run build-bundle
                ls -a build
                mkdir $out
                cp -r build/* $out
              '';

          # App that serves a static website - call with the static bundle like frontend-bundle-local.
          run-frontend-app = webapp:
            let
              port = 8080;
              name = "run-${project-name}-app";
              run = pkgs.writeShellApplication {
                inherit name;
                runtimeInputs = [
                  pkgs.nodePackages.http-server
                ];
                text = ''
                  http-server ${webapp} --port ${builtins.toString port}
                  #  --cors='*'
                '';
              };
            in
            {
              type = "app";
              program = "${run}/bin/${name}";
            };

          # Used to add pre-commit packages and shell hook to the other project shells
          mergeShells = devshell-1: devshell-2: pkgs.mkShell {
            packages = [ ];

            inputsFrom = [ devshell-1 devshell-2 ];

            shellHook = devshell-1.shellHook + devshell-2.shellHook;
          };

          # haskell development shell, with pre-commit shellhook
          onchain-devshell = mergeShells onchain.devShells.${system}.default config.pre-commit.devShell;
          # TAG: NIX_NODE_PATH
          # purescript development shell, with pre-commit shellhook
          offchain-devshell = mergeShells offchain.devShell config.pre-commit.devShell;
          # node development shell, with pre-commit shellhook.
          # Optional, alternatively install the same node+npm version from somewhere else 
          frontend-devshell =
            mergeShells
              # For shell it doesn't which offchain bundle gets included in src
              (frontend offchain-api-bundles.OffchainApiLocal).devShells.${system}.default
              config.pre-commit.devShell;

          # older ctl's nixpkgs, quick fix of ctl-runtime,
          # should be unneeded after https://github.com/Plutonomicon/cardano-transaction-lib/pull/1496
          pkgs-oldctl = import nixpkgs-oldctl {
            inherit system;
            overlays = [
              cardano-transaction-lib.overlays.purescript
              cardano-transaction-lib.overlays.runtime
              cardano-transaction-lib.overlays.spago
            ];
          };

          # purescriptProject provides app that serves documentation.
          # Because we don't pass just ./offchain as src the doc app breaks, this is a workaround.
          # https://github.com/Plutonomicon/cardano-transaction-lib/issues/1500
          docs = (pkgs.purescriptProject rec {
            inherit pkgs;
            projectName = "plutus-scaffold-offchain";
            strictComp = false;
            src = ./offchain;
            packageJson = "${src}/package.json";
            packageLock = "${src}/package-lock.json";
            spagoPackages = "${src}/spago-packages.nix";
          }).launchSearchablePursDocs { port = 9090; };

        in
        {

          packages =
            onchain.packages.${system}
            //
            rec {
              # TAG: SwitchContractParams
              frontend-bundle-local = frontend-bundle offchain-api-bundles.OffchainApiLocal;
              frontend-bundle-deployment = frontend-bundle offchain-api-bundles.OffchainApiDeployment;
              default = frontend-bundle-local;
            }
            // offchain-api-bundles
          ;

          checks =
            {
              psm-tests = onchain.checks.${system}."mlabs-plutus-template-onchain:test:psm-test";
              plutip-tests = offchain.runPlutipTest { testMain = "Test.Scaffold.Main"; };
            };

          devShells = {
            frontend = frontend-devshell;
            onchain = onchain-devshell;
            offchain = offchain-devshell;
          };

          apps =
            {
              # Run `nix run .#docs` and open `localhost:9090` to browse this projects documentation
              inherit docs;
              # Ctl's docs, at `localhost:8080`.
              ctl-docs = cardano-transaction-lib.apps.${system}.docs;
              # nix run .#script-exporter -- compiled-scripts
              script-exporter = {
                type = "app";
                program = self.packages.${system}.script-exporter.outPath;
              };
              # TAG: SwitchContractParams
              # The two below are buggy. For development use `npm run start` from frontend.
              run-frontend-app-local = run-frontend-app config.packages.frontend-bundle-local;
              run-frontend-app-deployment = run-frontend-app config.packages.frontend-bundle-deployment;
              ctl-runtime = pkgs-oldctl.launchCtlRuntime { };
              ctl-blockfrost-runtime = pkgs-oldctl.launchCtlRuntime { blockfrost.enable = true; };
            };

          hercules-ci.github-pages.settings.contents = config.packages.frontend-bundle-deployment;
        };
      flake = {

        hercules-ci.github-pages.branch = "main";
        # On CI, build only on available systems, to avoid errors about systems without agents.
        hercules-ci.ciSystems = [ "x86_64-linux" ];

        # Used by `nix flake init -t <flake>`
        templates.default = {
          path = ./.;
          description = "Mlabs Plutus project template";
          welcomeText = ''
            Welcome in the Plutus scaffold! 
            
            Check out the "Getting Started" section of README.
            
            To enter the Nix environment, run `nix develop .#onchain` or `nix develop .#offchain` respectively.
            Frontend is managed by npm, see the `frontend/package.json` scripts field.
            For offchain consult [ctl docs](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc).
          '';
        };
      };
    };
}
