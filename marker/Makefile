.PHONY: default
default: build test ## Builds debug version and run all tests.

.PHONY: build
build: ## Builds library source with rebar3
	rebar3 compile

.PHONY: test
test: ## Runs Eunit tests for all modules.
	rebar3 eunit

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
