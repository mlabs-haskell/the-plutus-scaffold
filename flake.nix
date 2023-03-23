{
  description = "Mlabs Plutus Template";

  inputs = {
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/b565f4b1ec877c671ec4ffc13b1b89dbe498bceb;
    # To use the same version of `nixpkgs` as ctl does
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, cardano-transaction-lib, ... }:
    let
      supportedSystems = [ "x86_64-linux" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          cardano-transaction-lib.overlays.purescript
          cardano-transaction-lib.overlays.runtime
          cardano-transaction-lib.overlays.spago
        ];
      };

      offchain = {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
          in
          pkgs.purescriptProject {
            inherit pkgs;
            projectName = "mlabs-plutus-template-project";
            strictComp = false; # TODO: this should be eventually removed
            src = ./offchain;
            shell = {
              withRuntime = true;
              packageLockOnly = true;
              packages = with pkgs; [
                fd
                nodePackages.eslint
                nodePackages.prettier
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

      offchain = perSystem offchain.projectFor;

      devShells = (perSystem (system: {
        default = self.offchain.${system}.devShell;
      }));
    };
}
