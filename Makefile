
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

format:
	pre-commit run --all-files

# TAG: compiled-scripts
# Run after you update your onchain scripts.
# Compiles the scripts into compiled-scripts-path directory
build-onchain-scripts:
	nix build .#exported-scripts -o ./compiled-scripts

# TAG: OFFCHAIN.JS
# Run after you update your offchain, bundles OffchainApiLocal
build-offchain-api:
	nix build .#OffchainApiLocal
	cp result/Offchain.js ./frontend/src/

# TAG: OFFCHAIN.JS
# Run after you update your offchain, bundles OffchainApiDeployment
build-offchain-api-deployment:
	nix build .#OffchainApiDeployment
	cp result/Offchain.js ./frontend/src/
