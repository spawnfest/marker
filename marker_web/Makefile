.PHONY: default
default: help ## Builds debug version and run all tests.

.PHONY: build
build: ## Builds library source with rebar3
	rebar3 compile

.PHONY: run
Run: ## Runs marker_web server.
	mix phx.server

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
