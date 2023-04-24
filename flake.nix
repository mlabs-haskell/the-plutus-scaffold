{
  description = "Mlabs Plutus Template";

  nixConfig = {
    allow-import-from-derivation = "true";
  };

  inputs = {
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/v5.0.0;
    mlabs-tooling.url = github:mlabs-haskell/mlabs-tooling.nix;
    flake-parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    ply.url = github:mlabs-haskell/ply?ref=0.4.0;
    plutarch.url = "github:Plutonomicon/plutarch-plutus?ref=95e40b42a1190191d0a07e3e4e938b72e6f75268";
    psm.url = github:mlabs-haskell/plutus-simple-model;

    # To use the same version of `nixpkgs` as we do
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, cardano-transaction-lib, mlabs-tooling, flake-parts, ... }:
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

          perSystem = { config, self', inputs', pkgs, system, ... }:
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
                script-exporter = script-exporter;
                exported-scripts = exported-scripts;
              };
            };
        };

      # ctl overlays
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          cardano-transaction-lib.overlays.purescript
          cardano-transaction-lib.overlays.runtime
          cardano-transaction-lib.overlays.spago
        ];
      };

      offchain = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.purescriptProject rec {
          inherit pkgs;
          projectName = "mlabs-plutus-template-project";
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
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # (import ./hercules-ci.nix)
        (import ./pre-commit.nix)
      ];
      inherit systems;
      perSystem = { system, config, ... }:
        {

          packages = onchain.packages.${system};

          checks = { };
          # onchain.checks.${system}
          # //
          # {
          #   plutipTests = (offchain system).runPlutipTest { testMain = "Test"; };
          # };

          devShells = {
            onchain = onchain.devShells.${system}.default;
            offchain = (offchain system).devShell;
          };

          apps =
            let
              # offchain pkgs
              pkgs = nixpkgsFor system;
            in
            {
              docs = (offchain system).launchSearchablePursDocs { };
              ctl-docs = cardano-transaction-lib.apps.${system}.docs;
              script-exporter = {
                # nix run .#script-exporter -- onchain-scripts
                type = "app";
                program = self.packages.${system}.script-exporter.outPath;
              };
              ctl-runtime = pkgs.launchCtlRuntime { };
              ctl-blockfrost-runtime = pkgs.launchCtlRuntime { blockfrost.enable = true; };
            };
        };
      flake = {
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
