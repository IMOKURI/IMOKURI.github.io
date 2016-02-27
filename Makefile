.PHONY: build rebuild watch

SITE_CMD = stack exec site

build:
	$(SITE_CMD) $@

rebuild: tmpdir := $(shell mktemp -d)
rebuild:
	mv _site/.git $(tmpdir)/.git
	$(SITE_CMD) $@
	mv $(tmpdir)/.git _site/.git
	rmdir $(tmpdir)

watch:
	$(SITE_CMD) $@

reset:
	$(SITE_CMD) clean
	git submodule init
	git submodule update
	git submodule foreach git pull origin master
