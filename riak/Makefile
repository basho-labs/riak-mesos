RIAK_REPO        ?= riak
RIAK_GIT         ?= https://github.com/basho/riak.git
RIAK_TAG         ?= riak-2.1.3
RIAK_SOURCE_DIR  ?= riak
RIAK_PKG_VERSION ?= 2.1.3
MAJOR            ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f1)
MINOR            ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f2)
ARCH             ?= amd64
OSNAME           ?= ubuntu
OSVERSION        ?= trusty
DEPLOY_BASE      ?= riak-tools/$(RIAK_REPO)/$(MAJOR).$(MINOR)/$(RIAK_PKG_VERSION)/$(OSNAME)/$(OSVERSION)/
PKGNAME           = $(RIAK_REPO)-$(RIAK_PKG_VERSION)-$(ARCH).tar.gz

.PHONY: all rel clean clean_package tarball sync

all: tarball

$(RIAK_SOURCE_DIR):
	git clone $(RIAK_GIT)
	cd $(RIAK_SOURCE_DIR) && git checkout $(RIAK_TAG)
rel: $(RIAK_SOURCE_DIR)
	cd $(RIAK_SOURCE_DIR) && $(MAKE) rel
clean_package:
	-rm -rf $(RIAK_BASE)
	-rm -rf packages
clean: clean_package
	-rm -rf $(RIAK_SOURCE_DIR)

##
## Packaging targets
##
RIAK_BASE        ?= root
tarball: rel
	echo "Creating packages/"$(PKGNAME)
	mkdir -p packages
	-rm -rf $(RIAK_BASE)
	mkdir -p $(RIAK_BASE)
	cp -R $(RIAK_SOURCE_DIR)/rel/riak $(RIAK_BASE)/
	tar -czf $(PKGNAME) $(RIAK_BASE) || rm -rf $(PKGNAME)
	mv $(PKGNAME) packages/
	cd packages && shasum -a 256 $(PKGNAME) > $(PKGNAME).sha
sync:
	echo "Uploading to "$(DEPLOY_BASE)
	cd packages && \
		s3cmd put --acl-public $(PKGNAME) s3://$(DEPLOY_BASE) && \
		s3cmd put --acl-public $(PKGNAME).sha s3://$(DEPLOY_BASE)
