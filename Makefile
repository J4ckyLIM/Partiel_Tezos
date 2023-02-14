ifndef LIGO
	LIGO = docker run --platform linux/amd64 --rm -v "${PWD}":"${PWD}" -w "${PWD}" ligolang/ligo:0.60.0
endif

default: help

compile = $(LIGO) compile contract ./src/contracts/$(1) -o ./src/compiled/$(2) $(3)
testing = $(LIGO) run test ./tests/$(1)

help:
	@echo "Usage: make [target]"
	@echo "Targets:"
	@echo "  compile: Compiles the contracts"
	@echo "  test:    Runs the tests"
	@echo "  clean:   Removes compiled contracts"

compile:
	@echo "Compiling..."
	@$(call compile,main.mligo,main.tz)
	@$(call compile,main.mligo,main.json, --michelson-format json)
	@echo "Compiled... Successful"

test: test-ligo test-integration

test-ligo: 
	@echo "Testing Ligo..."
	@$(call testing,invite_user.test.mligo)
	@echo "Testing Ligo... Successful"

deploy:
	@echo "Deploying..."
	@npm run deploy

test-integration:
	@echo "Testing integration..."

clean:
	@echo "Cleaning..."
	@rm -rf ./src/compiled/*

sandbox-start:
	@echo "Starting sandbox..."
		@./scripts/run-sandbox.sh

sandbox-stop:
	@echo "Stopping sandbox..."
	@docker stop sandbox
