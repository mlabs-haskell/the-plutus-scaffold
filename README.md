# Plutus Application Scaffold

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
- [History](#history)

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

#### What do the shells provide?

TODO: find more suitable place for this section. Maybe a whole section titled "How to eject from nix".

#### Building

The dependency flows in the order $\text{onchain} \rightarrow \text{offchain} \rightarrow \text{frontend}$,
that is a change in onchain affects the other two and the change in offchain affects just frontend.

Let's walk through a typical development scenario

##### Building onchain

Onchain is a haskell project managed by cabal. You can build the source by running

```
cabal build
```

from inside the `onchain` devshell.

This is helpfull to ensure project builds, but there's no effect of this command on offchain and frontend parts.
To propagate onchain script changes to downstream an [`exporter`](./onchain/exporter/Main.hs) executable is used.
You can run it with

```
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
``` inside to build the project.

This ensures the project builds, but to propagate the change down to the frontend we should instead run:

```sh
nix build .#bundle-offchain-api
mv result/Offchain.js ./frontend/src/
```

The command bundles offchain Api into a single js module and puts it into the frontend source.

##### Building frontend

Frontend is a react application written in typescript. It is managed fully by nodejs.
The first time after cloning the repository and after `frontend/package.json` changes run:

```sh
npm install
```

from inside the frontend directory and shell.

To start the development server serving the webpage run from inside the `frontend` directory:

```sh
npm run start
```

and check the webpage at [localhost:4008/](localhost:4008/). Don't forget to bundle offchain and export scripts before.

Or instead run

```sh
npm run build
```

to just build the webpage without serving it.

In frontend the nix provided devshell is of little importance - it just provides you with `node` and `npm` in path.
Instead you can use your user installed nodejs. The project was tested with nodejs in version v18.16.0 with npm in version 9.5.1.

> NOTE: All the `nix run/build/develop` commands can be run from any shell and anywhere in the repository (modulo relative paths). BUT all the other commands should be run from within the right devshells.

## History

This repository supersedes previous MLabs Plutus scaffold, but it doesn't share repository history with it.
Previous scaffold was a much more minimal scaffold, lacking ctl and frontend integration.
This project was kickstarted from [this repository](https://github.com/Mr-Andersen/ctl-multisign-mre).
