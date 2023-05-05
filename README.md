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

## History

This repository supersedes previous MLabs Plutus scaffold, but it doesn't share repository history with it.
Previous scaffold was a much more minimal scaffold, lacking ctl and frontend integration.
This project was kickstarted from [this repository](https://github.com/Mr-Andersen/ctl-multisign-mre).
