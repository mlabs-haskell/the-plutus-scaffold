# BIG TODO WIP

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

onchain-scripts-path := ./onchain-scripts
offchain-scripts-modules-path := offchain/src
scripts-psmodule-name := MLabsPlutusTemplate.Scripts

ps-sources := $$(fd -e purs)
ps-entrypoint := MLabsPlutusTemplate.Main
e2e-entrypoint := MLabsPlutusTemplate.Test.E2E.Serve
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js

build-onchain-scripts:
	nix build .#onchain-scripts -o ${onchain-scripts-path}

build-script-imports:
	spago run --main MLabsPlutusTemplate.ScriptImports -b ${onchain-scripts-path} ${offchain-scripts-modules-path} ${scripts-psmodule-name}

build-frontend:
	

run-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress

e2e-serve:
	spago bundle-module -m ${e2e-entrypoint} --to output.js
	BROWSER_RUNTIME=1 webpack-dev-server --progress

run-build:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --mode=production

check-format:
	@purs-tidy check ${ps-sources}

format:
	@purs-tidy format-in-place ${ps-sources}


bundle-frontend-api:
	spago bundle-module -m MlabsPlutusTemplate.Api --to ../frontend/src/Offchain.js