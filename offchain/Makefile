SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

e2e-entrypoint := Test.E2E.Serve
bundle-name := output.js

# TAG: SwitchContractParams OFFCHAIN.JS
# Run after you update your offchain, bundles OffchainApiLocal
bundle-offchain-api:
	spago bundle-module -m OffchainApiLocal --to ../frontend/src/Offchain.js

# TAG: SwitchContractParams OFFCHAIN.JS
# Run after you update your offchain, bundles OffchainApiDeployment
bundle-offchain-api-deployment:
	spago bundle-module -m OffchainApiDeployment --to ../frontend/src/Offchain.js

# Used in npm scripts
e2e-serve:
	spago bundle-module -m ${e2e-entrypoint} --to ${bundle-name}
	BROWSER_RUNTIME=1 webpack-dev-server --progress

# TAG: compiled-scripts
# Command that generates modules with script imports: Scripts.js and Scripts.purs, based on compiled-scripts.
# Should be considered a convenience and not a part of normal workflow. The two modules need only be updated, 
# when the set of script names changes (i.e. by addition of new script or a name change)
# 
# Flag --deps-only make spago not build the whole project avoiding chicken and egg problem, 
# where the executable needs to first run and write modules which are needed for compilation
# Feel free to generate the two modules once but later update them manualy, 
# if the problem gets out of hand. Or move the ScriptImports executable to seperate project.
generate-script-imports:
	spago run --deps-only -p exe/ScriptImports.purs --main ScriptImports -b ../compiled-scripts/ -b src -b Scripts