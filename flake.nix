{
  inputs = {
    plutip.url = github:mlabs-haskell/plutip/8364c43ac6bc9ea140412af9a23c691adf67a18b;
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/fcdd234cfe71345990f09eb1d6b4e2274faa2405;
    haskell-nix.follows = "plutip/haskell-nix";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutip, cardano-transaction-lib, ... }:
    let
      # GENERAL
      # supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      supportedSystems = [ "x86_64-linux" ];
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

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          nativeBuildInputs = [
            pkgs'.fd
            pkgs'.git
            pkgs'.nixpkgs-fmt
            pkgs.easy-ps.purs-tidy
            pkgs'.haskell.packages.${onchain.ghcVersion}.cabal-fmt
            pkgs'.haskell.packages.${onchain.ghcVersion}.fourmolu
          ];
          inherit (pkgs'.lib) concatStringsSep;
          otherBuildInputs = [ pkgs'.bash pkgs'.coreutils pkgs'.findutils pkgs'.gnumake pkgs'.nix ];
          format = pkgs.writeScript "format"
            ''
              export PATH=${concatStringsSep ":" (map (b: "${b}/bin") (otherBuildInputs ++ nativeBuildInputs))}
              export FOURMOLU_EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor"
              set -x
              purs-tidy format-in-place $(fd -epurs)
              fourmolu $FOURMOLU_EXTENSIONS --mode inplace --check-idempotence $(find onchain/{exporter,src} -iregex ".*.hs")
              nixpkgs-fmt $(fd -enix)
              cabal-fmt --inplace $(fd -ecabal)
            '';
        in
        {
          inherit format;
        }
      ;

      # ONCHAIN / Plutarch

      onchain = rec {
        ghcVersion = "ghc8107";

        inherit (plutip.inputs) nixpkgs haskell-nix;

        nixpkgsFor = system: import nixpkgs {
          inherit system;
          overlays = [
            haskell-nix.overlay
            (import "${plutip.inputs.iohk-nix}/overlays/crypto")
          ];
          inherit (haskell-nix) config;
        };
        nixpkgsFor' = system: import nixpkgs { inherit system; inherit (haskell-nix) config; };

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
          in
          pkgs.haskell-nix.cabalProject {
            src = ./onchain;
            compiler-nix-name = ghcVersion;
            index-state = "2022-05-25T00:00:00Z";
            cabalProject = ''
              packages: ./.
            '';
            inherit (plutip) cabalProjectLocal;
            extraSources = plutip.extraSources ++ [
              {
                src = "${plutip}";
                subdirs = [ "." ];
              }
            ];
            modules = plutip.haskellModules;
            shell = {
              withHoogle = false;
              exactDeps = true;
              nativeBuildInputs = with pkgs'; [
                git
                haskellPackages.apply-refact
                fd
                cabal-install
                hlint
                haskellPackages.cabal-fmt
                haskellPackages.fourmolu
                nixpkgs-fmt
              ];
              tools.haskell-language-server = { };
              additional = ps:
                with ps; [
                  cardano-api
                  plutus-ledger
                  plutus-ledger-api
                  plutus-script-utils
                  plutus-tx
                  plutus-tx-plugin
                  serialise
                ];
            };
          };

        script-exporter = system:
          let
            pkgs' = nixpkgsFor' system;
            exporter = ((projectFor system).flake { }).packages."mlabs-plutus-template-onchain:exe:exporter";
          in
          pkgs'.runCommandLocal "script-exporter" { }
            ''
              ln -s ${exporter}/bin/exporter $out
            '';

        exported-scripts = system:
          let
            pkgs' = nixpkgsFor' system;
            exporter = ((projectFor system).flake { }).packages."mlabs-plutus-template-onchain:exe:exporter";
          in
          pkgs'.runCommand "exported-scripts" { }
            ''
              set -e
              mkdir $out
              ${exporter}/bin/exporter
            '';
      };

      # OFFCHAIN / Testnet, Cardano, ...

      offchain = {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            exporter = ((onchain.projectFor system).flake { }).packages."mlabs-plutus-template-onchain:exe:exporter";
          in
          pkgs.purescriptProject {
            inherit pkgs;
            projectName = "mlabs-plutus-template-project";
            strictComp = false; # TODO: this should be eventually removed
            src = pkgs.runCommandLocal "generated-source" { }
              ''
                set -e
                cp -r ${./offchain} $out
                chmod -R +w $out
                ${exporter}/bin/exporter $out/src
              '';
            packageJson = ./offchain/package.json;
            packageLock = ./offchain/package-lock.json;
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

      onchain = {
        project = perSystem onchain.projectFor;
        flake = perSystem (system: (onchain.projectFor system).flake { });
      };

      offchain = {
        project = perSystem offchain.projectFor;
        flake = perSystem (system: (offchain.projectFor system).flake { });
      };

      packages = perSystem (system:
        self.onchain.flake.${system}.packages
        // {
          script-exporter = onchain.script-exporter system;
          exported-scripts = onchain.exported-scripts system;
        }
      );
      checks = perSystem (system:
        self.onchain.flake.${system}.checks
        // {
          mlabs-plutus-template = self.offchain.project.${system}.runPlutipTest { testMain = "Test"; };
        }
      );

      devShells = perSystem (system: {
        # why its either "flake" or "project" for the two?
        onchain = self.onchain.flake.${system}.devShell;
        offchain = self.offchain.project.${system}.devShell;
      });

      apps = perSystem (system: {
        docs = self.offchain.project.${system}.launchSearchablePursDocs { };
        ctl-docs = cardano-transaction-lib.apps.${system}.docs;
        script-exporter = {
          # nix run .#script-exporter -- offchain/src
          type = "app";
          program = (onchain.script-exporter system).outPath;
        };
        format = {
          type = "app";
          program = (formatCheckFor system).format.outPath;
        };
      });
    };
}
