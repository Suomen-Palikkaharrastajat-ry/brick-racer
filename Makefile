VITE ?= vite --configLoader native
WATCH_PORT ?= 5176
PREVIEW_PORT ?= 4176

.PHONY: help
help: ## Show available targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# ── Vendor / symlinks ────────────────────────────────────────────────────────

.PHONY: vendor
vendor: ## Init and update submodules and configure symlinks
	@[ -z "$$CI" ] || git config --global url."https://github.com/".insteadOf "git@github.com:"
	@if [ -d .git ]; then git submodule update --init --recursive; fi
	mkdir -p vendor review/src
	ln -sfn brick-studio/packages vendor/packages
	ln -sfn brick-studio/vendor/master-builder vendor/master-builder
	ln -sfn vendor/brick-studio/vendor/master-builder/packages packages
	ln -sfn ../../vendor/brick-studio/vendor/master-builder/review/src/LlmAgent review/src/LlmAgent

# ── Development environment ──────────────────────────────────────────────────

.PHONY: shell
shell: ## Enter devenv shell
	devenv shell

.PHONY: develop
develop: devenv.local.nix devenv.local.yaml ## Bootstrap devenv shell + VS Code
	devenv shell --profile=devcontainer -- code .

# ── Sources ──────────────────────────────────────────────────────────────────

SHARED_ELM_SOURCES := $(shell find vendor/packages/ -name '*.elm' 2>/dev/null)
RACER_SOURCES := $(shell find src -name '*.elm') $(SHARED_ELM_SOURCES)
ELM_PACKAGE_SOURCES := $(shell find packages/ -name '*.elm' -o -name '*.css' 2>/dev/null)

# ── Development & Build ──────────────────────────────────────────────────────

.PHONY: elm-tailwind-gen
elm-tailwind-gen: .elm-tailwind/.stamp ## Generate typed Tailwind Elm modules

.elm-tailwind/.stamp: elm.json vite.config.mjs main.css $(RACER_SOURCES) $(ELM_PACKAGE_SOURCES)
	elm-tailwind-classes gen
	mkdir -p .elm-tailwind
	touch $@

.PHONY: racer-dev
racer-dev: elm-tailwind-gen ## Start Vite dev server (http://localhost:5176)
	$(VITE) --host 0.0.0.0 --port $(WATCH_PORT)

.PHONY: dev
dev: racer-dev ## Alias for racer-dev

build/.stamp: elm-tailwind-gen index.html main.js audio.js main.css vite.config.mjs $(RACER_SOURCES) $(ELM_PACKAGE_SOURCES)
	mkdir -p build
	$(VITE) build --outDir build
	touch $@

.PHONY: racer-build
racer-build: build/.stamp ## Production build of brick-racer → build/

.PHONY: build
build: racer-build ## Build brick-racer

.PHONY: preview
preview: build ## Preview production build locally
	$(VITE) preview --outDir build --host 0.0.0.0 --port $(PREVIEW_PORT)

.PHONY: serve
serve: preview ## Alias for preview

# ── Test & Quality ───────────────────────────────────────────────────────────

.PHONY: elm-test
elm-test: elm-tailwind-gen ## Run all unit tests
	elm-test

.PHONY: test
test: check elm-test ## Run check and racer tests

.PHONY: elm-check
elm-check: ## Check Elm formatting (no changes)
	elm-format --validate src/ tests/ || true

.PHONY: elm-format
elm-format: ## Auto-format Elm source files
	elm-format --yes src/ tests/

.PHONY: elm-review
elm-review: ## Run elm-review rules
	elm-review

.PHONY: elm-review-fix
elm-review-fix: ## Run elm-review with auto-fix
	elm-review --fix

.PHONY: check
check: elm-check elm-review ## Validate Elm formatting and review rules

.PHONY: format
format: elm-format ## Auto-format source files
	treefmt

# ── Cleanup ───────────────────────────────────────────────────────────────────

.PHONY: clean
clean: ## Remove build artifacts
	rm -rf build .elm-tailwind elm-stuff .vite
