BASE_DIR                    = $(PWD)
riak-mesos-scheduler_TAG   ?= 0.0.1
riak-mesos-executor_TAG    ?= 0.1.2
riak-mesos-director_TAG    ?= 0.3.1
riak_explorer_TAG          ?= 0.1.1-patch

export ARCH                ?= amd64
export OSNAME              ?= ubuntu
export OSVERSION           ?= trusty

.PHONY: all deps clean updatehead

all: updatehead tarball

tarball:
	$(foreach dep,$(shell ls deps), cd $(BASE_DIR)/deps/$(dep) && $(MAKE) tarball;)
	cd riak && $(MAKE) tarball

deps:
	git submodule update --init --recursive
	$(foreach dep,$(shell ls deps), cd $(BASE_DIR)/deps/$(dep) && git checkout master && git pull;)

clean:
	$(foreach dep,$(shell ls deps), cd $(BASE_DIR)/deps/$(dep) && $(MAKE) clean && git reset --hard HEAD;)
	cd riak && $(MAKE) clean

updatehead: deps
	$(foreach dep,$(shell ls deps), cd $(BASE_DIR)/deps/$(dep) && git fetch origin $($(dep)_TAG) && git checkout $($(dep)_TAG);)
