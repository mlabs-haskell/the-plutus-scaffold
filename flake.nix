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
  };

  outputs = inputs@{ self, nixpkgs, cardano-transaction-lib, mlabs-tooling, flake-parts, nixpkgs-oldctl, dream2nix, ... }:
    let
      systems = [ "x86_64-linux" ];

      # ONCHAIN / Plutarch
      onchain = mlabs-tooling.lib.mkFlake { inherit self; }
        {
          imports = [
            (mlabs-tooling.lib.mkHaskellFlakeModule1 {
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

          inherit systems;

          perSystem = { self', pkgs, ... }:
            let
              exporter = self'.packages."mlabs-plutus-template-onchain:exe:exporter";

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

      # Used to override pre-commit hooks install script, to make all shells install the same pre-commit script. 
      appendShellHook = shell: hook: shell.overrideAttrs (finalAttrs: finalAttrs // {
        # we override the shellhook to install correct pre-commit hooks
        shellHook =
          finalAttrs.shellHook  # we first run shell's own hook
            + hook; # then run hook that installs pre-commit (and hope the two hooks are not in conflict)
      });
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

          projectName = "plutus-scaffold-offchain";

          offchain =
            pkgs.purescriptProject rec {
              inherit pkgs;
              inherit projectName;
              # If warnings generated from project source files will trigger a build error
              strictComp = false;
              src = builtins.path {
                path = ./offchain;
                name = "${projectName}-src";
              };
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

          # TAG: Offchain-Api-Bundle-Name
          bundledModuleName = "Offchain.js";

          # Bundles with `spago bundle-module`, sharing the built project with the offchain purescriptProject
          bundlePsModule =
            { main ? "Main" }:
            let
              name = "${projectName}-bundle-${main}";
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

          # Derivation producing offchain api bundle Offchain.js
          # Basically `spago bundle-module -m Api --to Offchain.js`
          bundled-offchain-api = bundlePsModule { main = "Api"; };

          # Derivation producing frontend's src with the offchain api bundle linked at src/Offchain.js
          # This should match your local tracked frontend directory
          frontendFullSrc = pkgs.runCommand "plutus-scaffold-frontend-full-src" { } ''
            mkdir res
            cp -r ${./frontend}/* res
            chmod +w res/src
            cp ${bundled-offchain-api}/${bundledModuleName} res/src
            mkdir $out
            cp -r res/* $out
          '';

          # frontend flake outputs
          frontend = dream2nix.lib.makeFlakeOutputs {
            systems = [ system ];
            config.projectRoot = ./frontend;
            source = frontendFullSrc;
            projects = ./frontend/project.toml;
          };

          # haskell development shell, with pre-commit shellhook
          onchain-devshell = appendShellHook onchain.devShells.${system}.default config.pre-commit.installationScript;
          # purescript development shell, with pre-commit shellhook
          offchain-devshell = appendShellHook offchain.devShell config.pre-commit.installationScript;

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

          packages = {
            inherit bundled-offchain-api;
          }
          // onchain.packages.${system}
          // frontend.packages.${system};

          checks = { };

          devShells = {
            frontend = frontend.devShells.${system}.default;
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
