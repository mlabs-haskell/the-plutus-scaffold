SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $$(fd -e purs)
ps-entrypoint := Main
e2e-entrypoint := Test.E2E.Serve
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js # index.js?
# TAG: compiled-scripts - directory onchain scripts are saved to
compiled-scripts-dir := compiled-scripts

run-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress

e2e-serve:
	spago bundle-module -m ${e2e-entrypoint} --to output.js
	BROWSER_RUNTIME=1 webpack-dev-server --progress

run-build:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack --mode=production

format:
	@purs-tidy format-in-place ${ps-sources}

# Flag --deps-only make spago not build the whole project avoiding chicken and egg problem, 
# where the executable needs to first run and write modules which are needed for compilation
# Feel free to generate the two modules once but later update them manualy, 
# if the problem gets out of hand. Or move the ScriptImports executable to seperate project.
generate-script-imports:
	spago run --deps-only -p exe/ScriptImports.purs --main ScriptImports -b ../compiled-scripts/ -b src -b Scripts

bundle-frontend-api:
	spago bundle-module -m Api --to ../frontend/src/Offchain.js
