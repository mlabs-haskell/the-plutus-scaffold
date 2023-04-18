SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $$(fd -e purs)
ps-entrypoint := Main
e2e-entrypoint := Test.E2E.Serve
ps-bundle = spago bundle-module -m ${ps-entrypoint} --to output.js

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

generate-script-imports:
	spago run --main ScriptImports -b ../compiled-scripts/ -b src

bundle-frontend-api:
	spago bundle-module -m Api --to ../frontend/src/Offchain.js