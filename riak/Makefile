RIAK_TAG         ?= 
RIAK_SOURCE_DIR  ?= 
RIAK_REPO        ?= $(shell echo $(RIAK_TAG) | cut -d'-' -f1)
RIAK_PKG_VERSION ?= $(shell echo $(RIAK_TAG) | cut -d'-' -f2)
MAJOR            ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f1)
MINOR            ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f2)
ARCH             ?= amd64
OSNAME           ?= ubuntu
OSVERSION        ?= trusty
S3_BASE         ?= riak-tools
S3_PREFIX       ?= http://$(S3_BASE).s3.amazonaws.com/
DEPLOY_BASE     ?= $(RIAK_REPO)/$(MAJOR).$(MINOR)/$(RIAK_PKG_VERSION)/$(OSNAME)/$(OSVERSION)/
PKGNAME         ?= $(RIAK_REPO)-$(RIAK_PKG_VERSION)-$(ARCH).tar.gz
RIAK_BASE        ?= root

.PHONY: all tarball clean sync

all: tarball

tarball:
	echo "Creating packages/"$(PKGNAME)
	cd $(RIAK_SOURCE_DIR) && $(MAKE) rel
	mkdir -p packages
	-rm -rf $(RIAK_BASE)
	mkdir -p $(RIAK_BASE)
	cp -R $(RIAK_SOURCE_DIR)/rel/riak $(RIAK_BASE)/
	tar -czf $(PKGNAME) $(RIAK_BASE) || rm -rf $(PKGNAME)
	mv $(PKGNAME) packages/
	cd packages && shasum -a 256 $(PKGNAME) > $(PKGNAME).sha
	cd packages && echo "$(S3_PREFIX)$(DEPLOY_BASE)$(PKGNAME)" > remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PKGNAME)" > local.txt

clean:
	-rm -rf $(RIAK_BASE)
	-rm -rf packages

sync:
	echo "Uploading to "$(DEPLOY_BASE)
	cd packages && \
		s3cmd put --acl-public $(PKGNAME) s3://$(S3_BASE)/$(DEPLOY_BASE) && \
		s3cmd put --acl-public $(PKGNAME).sha s3://$(S3_BASE)/$(DEPLOY_BASE)
