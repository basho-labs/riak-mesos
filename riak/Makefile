BASE_DIR          = $(PWD)
RIAK_SOURCE_DIR  ?= riak
RIAK_TAG         ?= $(shell cd $(RIAK_SOURCE_DIR) && git describe --tags)
RIAK_REPO        ?= $(shell echo $(RIAK_TAG) | cut -d'-' -f1)
RIAK_PKG_VERSION ?= $(shell echo $(RIAK_TAG) | cut -d'-' -f2)
OS_FAMILY        ?= ubuntu
OS_VERSION       ?= 14.04
PKGNAME          ?= $(RIAK_REPO)-$(RIAK_PKG_VERSION)-$(OS_FAMILY)-$(OS_VERSION).tar.gz
RIAK_BASE        ?= root
OAUTH_TOKEN      ?= $(shell cat oauth.txt)
RIAK_MESOS_TAG   ?= $(shell git describe --tags --abbrev=0)
RELEASE_ID       ?= $(shell curl -sS https://api.github.com/repos/basho-labs/riak-mesos/releases/tags/$(RIAK_MESOS_TAG)?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print json.load(sys.stdin)["id"]')
DEPLOY_BASE      ?="https://uploads.github.com/repos/basho-labs/riak-mesos/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN)&name=$(RIAK_PKGNAME)"
DOWNLOAD_BASE    ?=https://github.com/basho-labs/riak-mesos/releases/download/$(RIAK_MESOS_TAG)/$(PKGNAME)

.PHONY: all tarball clean sync

all: tarball

ifneq (,$(shell whereis sha256sum | awk '{print $2}';))
SHASUM = sha256sum
else
SHASUM = shasum -a 256
endif

tarball:
	echo "Creating packages/"$(PKGNAME)
	cd $(RIAK_SOURCE_DIR) && $(MAKE) rel
	mkdir -p packages
	-rm -rf $(RIAK_BASE)
	mkdir -p $(RIAK_BASE)
	cp -R $(RIAK_SOURCE_DIR)/rel/riak $(RIAK_BASE)/
	tar -czf $(PKGNAME) $(RIAK_BASE) || rm -rf $(PKGNAME)
	mv $(PKGNAME) packages/
	cd packages && $(SHASUM) $(PKGNAME) > $(PKGNAME).sha
	cd packages && echo "$(DOWNLOAD_BASE)" > remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PKGNAME)" > local.txt

clean:
	-rm -rf $(RIAK_BASE)
	-rm -rf packages

sync:
	echo "Uploading to "$(DOWNLOAD_BASE)
	@cd packages && \
		curl -sS -XPOST -H 'Content-Type: application/gzip' "$(DEPLOY_BASE)" --data-binary @$(PKGNAME) && \
		curl -sS -XPOST -H 'Content-Type: application/octet-stream' "$(DEPLOY_BASE).sha" --data-binary @$(PKGNAME).sha
