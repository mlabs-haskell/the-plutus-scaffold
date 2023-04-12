{}: {
  src = ./.;
  settings = {
    # FIXME: https://github.com/cachix/pre-commit-hooks.nix/issues/155
    ormolu.defaultExtensions = [
      "TypeApplications"
      "QualifiedDo"
      "OverloadedRecordDot"
      "NonDecreasingIndentation"
    ];
  };

  hooks = {
    nixpkgs-fmt.enable = true;
    # nix-linter.enable = true;
    cabal-fmt.enable = true;
    fourmolu.enable = true;
    shellcheck.enable = true;
    hlint.enable = true;
    # Be careful if you enable typos,  might try to spellcheck a binary file or PDF -_-
    # typos.enable = true;
    # markdownlint.enable = true;
    dhall-format.enable = true;
    purs-tidy.enable = true;
    tsfmt = {
      enable = true;
      name = "tsfmt";
      description = "Typescript code formatter";
      entry = "tsfmt -r";
      files = "\\.(ts|tsx)$";
    };
  };
}
