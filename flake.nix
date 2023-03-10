{
  description = "Mlabs Plutus Template";

  nixConfig = {
    # extra-substituters = [ "https://cache.iog.io" ];
    # extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    plutip.url = github:mlabs-haskell/plutip/8364c43ac6bc9ea140412af9a23c691adf67a18b;
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/fcdd234cfe71345990f09eb1d6b4e2274faa2405;
    haskell-nix.follows = "plutip/haskell-nix";
    tooling.url = github:mlabs-haskell/mlabs-tooling.nix;

    # onchain plutarch
    # TODO: nixpkg follows?
    ply.url = github:mlabs-haskell/ply?ref=0.4.0;
    plutarch.url = "github:Plutonomicon/plutarch-plutus?ref=95e40b42a1190191d0a07e3e4e938b72e6f75268";
    psm.url = github:mlabs-haskell/plutus-simple-model;

  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutip, cardano-transaction-lib, tooling, ... }:
    let
      # GENERAL
      # supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      supportedSystems = [ "x86_64-linux" ];

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

          systems = [ "x86_64-linux" ];

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
                    set -e
                    mkdir $out
                    ${exporter}/bin/exporter
                  '';
            in
            {
              packages = {
                script-exporter = script-exporter;
                exported-scripts = exported-scripts;
              };
            };
        };

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          haskell-nix.overlay
          cardano-transaction-lib.overlays.purescript
          cardano-transaction-lib.overlays.runtime
        ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      # OFFCHAIN / Testnet, Cardano, ...

      offchain = {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            exporter = onchain-plutarch.packages.${system}."mlabs-plutus-template-onchain:exe:exporter";
          in
          pkgs.purescriptProject {
            inherit pkgs;
            projectName = "mlabs-plutus-template-project";
            strictComp = false; # TODO: this should be eventually removed
            src = ./offchain;
            shell = {
              packageLockOnly = true;
              packages = with pkgs; [
                bashInteractive
                docker
                fd
                nodePackages.eslint
                nodePackages.prettier
                ogmios
                ogmios-datum-cache
                plutip-server
                postgresql
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
      };
    in
    {
      inherit nixpkgsFor;

      offchain = {
        project = perSystem offchain.projectFor;
        flake = perSystem (system: (offchain.projectFor system).flake { });
      };

      packages = onchain-plutarch.packages;

      checks = onchain-plutarch.checks
        // (perSystem (system:
        {
          mlabs-plutus-template = self.offchain.project.${system}.runPlutipTest { testMain = "Test"; };
        }
      ));

      devShells = (perSystem (system: {
        onchain = onchain-plutarch.devShells.${system}.default;
        offchain = self.offchain.project.${system}.devShell;
        default = onchain-plutarch.devShells.${system}.default;
      }));

      apps = perSystem (system: {
        docs = self.offchain.project.${system}.launchSearchablePursDocs { };
        ctl-docs = cardano-transaction-lib.apps.${system}.docs;
        script-exporter = {
          # nix run .#script-exporter -- offchain/src
          type = "app";
          program = onchain-plutarch.script-exporter.outPath;
        };
        # ctl-runtime = self.offchain.project.${system}.;
        # format = {
        #   type = "app";
        #   # program = (formatCheckFor system).format.outPath;
        # };
      });
    };
}
