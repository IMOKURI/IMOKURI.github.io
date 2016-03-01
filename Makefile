.PHONY: build rebuild watch reset help
.DEFAULT_GOAL := help

SITE_CMD = stack exec site

build: ## Build Hakyll site code
	$(SITE_CMD) $@

rebuild: tmpdir := $(shell mktemp -d) ## Rebuild Hakyll site code
rebuild:
	mv _site/.git $(tmpdir)/.git
	$(SITE_CMD) $@
	mv $(tmpdir)/.git _site/.git
	rmdir $(tmpdir)

watch: ## Preview Hakyll website
	$(SITE_CMD) $@

reset: ## Reset Hakyll site code directory
	$(SITE_CMD) clean
	git submodule init
	git submodule update
	git submodule foreach git pull origin master

help: ## This help
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[32m%-15s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
