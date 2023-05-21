# Plutus Application Scaffold

[![Hercules-ci][herc badge]][herc link]
[WebApp][gh page link]

[herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[herc link]: https://hercules-ci.com/github/mlabs-haskell/plutus-scaffold
[gh page link]: https://mlabs-haskell.github.io/plutus-scaffold/

The project provides a template to kickstart dapp development on Cardano.
This is aimed at helping developers quickly start implementing the dapp logic, instead of mingling with build tools, setup and integration of various tools.
Typically this would take more time than needed, because developers are not always experienced with all of the different components: the ctl library, nix, frontend development.
The project is an example of a dapp. Developers wanting to use the project are expected to check the project documentation
and later modify its source, possibly removing a lot of code, to match their needs.
Plutus scaffold is an opinionated example - it makes choices about tools and libraries, see a [list](#tools-list).
Though hopefully it's easy enough for users to change some of the choices, without need for bigger change of the project structure.

## Table of Contents

- [Tools list](#tools-list)
- [Overview](#overview)
  - [What does the app do?](#what-does-the-app-do)
  - [Onchain](#onchain)
  - [Offchain](#offchain)
  - [Frontend](#frontend)
- [Getting started](#getting-started)
  - [Installing nix](#installing-nix)
  - [Building and development](#building-and-development)
    - [Development shells](#development-shells)
    - [Direnv integration](#direnv-integration)
    - [Duilding](#building)
    - [Available commands](#available-commands)
    - [Testing](#testing)
- [Other](#other)
  - [About ContractParams](#about-contractparams)
  - [About bundling Purescript](#about-bundling-purescript)
  - [History](#history)
  - [FAQ](#faq)

## Tools list

- Onchain: alternative between [Plutarch](https://github.com/Plutonomicon/plutarch-plutus) and [PlutusTx](https://plutus.readthedocs.io/en/latest/tutorials/plutus-tx.html)
- [cardano-transaction-lib](https://github.com/Plutonomicon/cardano-transaction-lib) and purescript
- testing: [plutus-simple-model](https://github.com/mlabs-haskell/plutus-simple-model), plutip (via [cardano-transaction-lib plutip-testing](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md))
- frontend: [react](https://react.dev/), [webpack](https://webpack.js.org/), [typescript](https://www.typescriptlang.org/)
- [nix](https://nixos.org/)

## Overview

Project demonstrates the full spectrum of dapp development: from onchain scripts to the webapp gui, split into 3 parts: `./onchain`, `./offchain` and `./frontend`.

Keywords: Plutarch, PlutusTx, CTL, React

### What does the app do?

The dapp involves two onchain scripts: basic password validator and a minting policy. User interacts with the app
through a webpage (check out yourself at [link-todo](TODO)). User can lock funds at the validator script and then unlock those funds by providing the password in the Redeemer. User can also mint a token or burn the same token.

### Onchain

Onchain defines Plutus validation scripts. Project provides two alternative examples. One where validators are written in[Plutarch](https://github.com/Plutonomicon/plutarch-plutus) and the second using [PlutusTx](https://plutus.readthedocs.io/en/latest/tutorials/plutus-tx.html). The role of onchain is to output a directory like:

```
/
├── 493177bb4111452fac2d8abe7f8f79e800eec4cba24f1d53ae3efa3c.plutus
├── fc61e623d413aa67dc9367e8e48f5ab7f38093e871af6d9dd27b717e.plutus
└── Index.json
```

where `.plutus` files contain scripts compiled down to Plutus Core in the text file envelope format (see [docs/script-format.md](docs/script-format.md)) and `Index.json` provides a mapping of human readable script names to the script hashes.
The scripts directory is then consumed by offchain.

### Offchain

Offchain is a purescript library based on [CTL](https://github.com/Plutonomicon/cardano-transaction-lib) template.
It defines various blockchain related actions, that ought to happen on user interactions, described as ctl's `Contract` type.
An example is the `payToPassword` contract, which executes when user wants to lock funds at the script address. Executing the contract submits a transaction. The transaction pays specified funds from user's wallet to the script address and waits for transaction confirmation.

CTL contracts are meant to be executed in a browser environment with cardano wallet installed as a browser extension.
The other runtime requirement is availability of a few endpoints (used for querying blockchain) which we collectively call ctl-runtime. For it's description see [ctl-runtime](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/runtime.md). Spawning the runtime is not the contracts job. For overview on how to provide the runtime in the context of this project see [todo/docs/ctl-runtime.md](docs/ctl-runtime.md).

Offchain in essence defines queries and tx building required to interact with the onchain scripts. Its role is to export some minimal API to be consumed by the frontend. In practice this means outputting a javascript module, ready for import.

### Frontend

Frontend is a react based web application written in typescript. It plugs the various actions defined in the offchain to buttons in the GUI. Note that ctl-runtime is the only 'server' component, otherwise the webpage is static.

Product of frontend is the webpage bundle, ready to be hosted and opened by the user.

## Getting started

### Installing Nix

This repository relies on the [Nix Package
Manager](https://nixos.org/download.html) for both development and package
distribution.

To install nix run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

```sh
$ nix --version
nix (Nix) 2.8.0
```

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes)
and IFD by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes
allow-import-from-derivation = true
```

Optionally, to dramatically improve build speed, it is possible to set up a binary caches
maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://iohk.cachix.org https://cache.iog.io https://public-plutonomicon.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=
```

### Building and development

#### Development shells

Project provides development shells containing the needed tools:

- `onchain`
- `offchain`
- `frontend`
- `dev-pre-commit`

To enter the shell run:

```sh
nix develop .#<shell name>
```

i.e. `nix develop .#onchain`

> NOTE: On zsh you may need to run `nix develop .\#onchain` to prevent globbing.

Running `nix develop` lands in the default `dev-pre-commit` shell, allowing you to run

```sh
$ pre-commit run --all
cabal-fmt............................................(no files to check)Skipped
fourmolu.................................................................Passed
hlint....................................................................Passed
markdownlint.............................................................Passed
nix-linter...............................................................Passed
nixpkgs-fmt..............................................................Passed
shellcheck...........................................(no files to check)Skipped
typos....................................................................Passed
```

To run all the code quality tooling specified in the [pre-commit-check config file](./pre-commit.nix)

#### Direnv integration

To facilitate seamlessly moving between directories and associated Nix
development shells we recommend [direnv](https://direnv.net) and
[nix-direnv](https://github.com/nix-community/nix-direnv):

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

Direnv is a tool that loads devshell when entering a directory.
It also caches the shell, speeding up its loading.
Direnv looks for files named `.envrc` in the current directory.
Use the `.envrc.example` files provided in the repository.
After installing direnv it suffices to run

```sh
cp .envrc.example .envrc && direnv allow .envrc
```

in all the directories `./.`, `./onchain`, `./offchain` and `./frontend`.

#### Building

The dependency flows in the order $\text{onchain} \rightarrow \text{offchain} \rightarrow \text{frontend}$,
that is a change in onchain affects the other two and the change in offchain affects just frontend.

Let's walk through a typical development scenario.

##### Building onchain

Onchain is a haskell project managed by cabal. You can build the source by running

```
cabal build
```

from inside the `onchain` devshell.

This is helpfull to ensure project builds, but there's no effect of this command on offchain and frontend parts.
To propagate onchain script changes to downstream an [`exporter`](./onchain/exporter/Main.hs) executable is used.
You can run it with

```sh
nix build .#exported-scripts -o ./compiled-scripts
```

The command doesn't need to be run from any shell, but it should be run from the toplevel directory, or otherwise the
`./compiled-scripts` path would be wrong. Additionaly `make build-onchain-scripts` runs the above command.

The exporter saves onchain Plutus scripts into the format digestible by offchain. After running the command you project should look something like:

```yaml
.
  - compiled-scripts/
    - Index.json
    - always_succeeds.plutus
    - my_validator.plutus
  - onchain/
  - offchain/
  - frontend/
  - ...
```

##### Building offchain

If your onchain scripts changed names, you should update the way they are imported inside [./offchain/src/Scripts.js](./offchain/src/Scripts.js) and [./offchain/src/Scripts.purs](./offchain/src/Scripts.purs). Otherwise it suffices to run the exporter executable in the previous step.

Offchain is a purescript project managed by spago. Enter the `offchain` devshell and directory and run

```sh
spago build
```

This ensures the project builds, but to propagate the change down to the frontend we should instead run:

```sh
nix build .#OffchainApiLocal
cp result/Offchain.js ./frontend/src/
```

Provide correct relative path to the frontend's src directory.
The command bundles offchain Api into a single js module and puts it into the frontend source.
From toplevel directory you can use the `make build-offchain-api` helper.

The above commands bundle the Api module `OffchainApiLocal`. There's alternative module `OffchainApiDeployment` bundled with an according command. See the tag `TAG: ChooseContractParams` for explanation.

##### Building frontend

Frontend is a react application written in typescript. It is managed by nodejs.
The first time after cloning the repository and after `frontend/package.json` changes run:

```sh
npm install
```

from inside the frontend directory and shell.

To build the webpage run:

```sh
npm run build-bundle
```

Don't forget to bundle offchain and export scripts before.

Or run

```sh
nix build .#frontend-bundle-local # or frontend-bundle-deployment
```

producing the same result.

Instead to build and start the development server serving the webpage run from inside the `frontend` directory:

```sh
npm run start
```

and check the webpage at [localhost:4008/](localhost:4008/). For the webapp to function properly you also need to run the `ctl-runtime`. Start it first with

```
nix run .#ctl-runtime
```

In frontend the nix provided devshell is of little importance - it just provides you with `node` and `npm` in path.
Instead you can use your user installed nodejs. The project was tested with nodejs in version `14.20.1` with npm in version `6.14.17`.

##### Full build

The two commands:

```sh
nix build .#frontend-bundle-local
```

and

```sh
nix build .#frontend-bundle-deployment
```

build the 3 parts in order and produce the final webapp (a static bundle).

> NOTE: All the `nix run/build/develop` commands can be run from any shell and anywhere in the repository (modulo relative paths). BUT all the other commands should be run from within the right devshells.

#### Available commands

Some of the common commands are provided for convenience. See the toplevel [Makefile](./Makefile), for wrappers of the nix commands. Then Makefiles inside onchain and offchain directories, define same named commands (and extra), only outside of nix. Those commands need to be run from the nix shells, but in turn will often be faster. Then there's npm scripts defined in `package.json` files inside offchain and frontend, those should be treated like Makefile commands.

Related pieces of configuration are tagged with a comment

```
TAG: <TAG-IDENTIFIER>
```

trying to fight the non-locality of some the settings.

#### Testing

Run the tests listed below with `nix flake check`. E2e ctl tests are not included!
The default CI job runs flake checks and builds all flake outputs.

##### Onchain tests

Onchain defines a [plutus-simple-model](https://github.com/mlabs-haskell/plutus-simple-model) test suite. Run it with:

```sh
cabal test
```

##### Offchain tests

Offchain defines a plutip test suite, run with

```sh
npm run tests
```

and an e2e test suite. This is an important test suite - it may be defined to run on public testnet and to use real wallets. The test specification is in [e2e.env](./offchain/test/e2e.env). The tests needing a wallet setup are commented out, running the test suite now runs on plutip network with mock wallets. See ctl's instruction on  [plutip tests](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md) and [e2e testing](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/e2e-testing.md).

To run the e2e tests navigate to the offchain directory and:

1. run `npm run e2e-serve` in one shell
2. run `nix run -L .#ctl-runtime` in another shell
3. run `npm run e2e-test` in the third shell

##### Frontend tests

Frontend lacks any tests for now. Running, from within frontend directory and shell, `npm run build-bundle` is a good way of assuring that build artifacts provided by offchain and onchain gets imported correctly.

## Other

### About ContractParams

TAG: SwitchContractParams

`ContractParams` is ctl's datatype defining environent for contract execution.
Importantly this contains:

- Mainnet or Testnet choice
- urls to query services (Ogmios, Kupo, optionally Blockfrost)
- wallet to connect to (just a "brand" name)

We define two alternative, simmilar offchain modules, which define different `ContractParams`, for local testing and deployment purpose.
In the GUI we provide user with a choice that instantiates the wallet argument.

### About bundling Purescript

Purescript compiles to javascript and as such can be imported from javascript and can import javascript itself. Spago compiles every module to javascript producing `output` directory inside offchain. Running `spago bundle-module` bundles all of these js modules into a single self-contained js module.

I want to expand on self-contained: the bundle contains the whole referenced purescript source code, in compiled form. It does contain the javascript ffi modules (the ones with named the same as matching purescript modules, like `Scripts.js`) from our project and dependencies. But it doesn't contain javascript dependencies. That is the produced bundle will contain the import statements from our javascript code like all the `require('Scripts/always_succeeds.plutus')` from `Scripts.js`.
Same goes for `require` statements from the imported ctl's source. This is important, because
ctl uses `BROWSER_RUNTIME` environment variable to specify conditional imports. This means that when the spago bundle gets consumed in frontend by webpack, to produce the final fully-linked fully self-contained bundle, webpack needs to package simmilarly to how it's done in ctl. Ctl's node libraries need to be present and `BROWSER_RUNTIME` set. Not to multiple things, we use the same environment variable to guide our bundling.

As the spago bundles contain all the referenced purescript definitions - it means that using two different bundles made with `spago bundle-module` in a single js code is likely to fail. One has to eject from spago and bundle with some general purpose bundler.

### History

This repository supersedes previous MLabs Plutus scaffold, but it doesn't share repository history with it.
Previous scaffold was a much more minimal scaffold, lacking ctl and frontend integration.
This project was kickstarted from [this repository](https://github.com/Mr-Andersen/ctl-multisign-mre) and
[ctl-scaffold](https://github.com/Plutonomicon/cardano-transaction-lib/tree/v5.0.0/templates/ctl-scaffold).
The `offchain` directory closely mimicks ctl-scaffold, only builds custom app on top of it.

### FAQ

#### I'm getting "zsh: no matches found: " error when I run nix commands, what to do?

`Zsh` may treat `#` as a globbing pattern (option 'extendedglob'). Prefix `#` with slash `\` or disable the zsh option.

```zsh
nix develop .\#onchain
```

#### I'm getting "Output directory does not exist" from my purescript editor extension, what to do?

That's likely with vscode and the purescript-ide extenstion. `Spago` expects the project to be build into `output`, but opening you editor from the project toplevel directory it's at `offchain/output`. You can set it in the extensions settings or open the project from offchain.

#### I renamed `npm run build-bundle` to `npm run build` and now nix doesn't build, why?

The dream2nix library used for packaging frontend will run the command named `build` at build step, you may or may not not want that.
