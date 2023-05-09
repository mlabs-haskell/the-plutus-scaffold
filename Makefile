
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

compiled-scripts-path := ./compiled-scripts
# offchain-scripts-modules-path := offchain/src
# scripts-psmodule-name := MLabsPlutusTemplate.Scripts

build-onchain-scripts:
	nix build .#exported-scripts -o ${compiled-scripts-path}

# build-script-imports:
# 	spago run --main MLabsPlutusTemplate.ScriptImports -b ${onchain-scripts-path} ${offchain-scripts-modules-path} ${scripts-psmodule-name}

build-frontend:
	nix build .#bundle-offchain-api
	mv result/Offchain.js ./frontend/src/
