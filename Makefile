BASE_DIR                    = $(PWD)
riak-mesos-scheduler_TAG   ?= 0.0.1
riak-mesos-executor_TAG    ?= 0.1.2
riak-mesos-director_TAG    ?= 0.3.1
riak_explorer_TAG          ?= 0.1.1-patch

export ARCH                ?= amd64
export OSNAME              ?= ubuntu
export OSVERSION           ?= trusty

.PHONY: all deps clean updatehead

all: deps updatehead tarball

tarball:
	$(foreach dep,$(shell ls framework), cd $(BASE_DIR)/framework/$(dep) && $(MAKE) tarball;)
	cd riak && $(MAKE) tarball

deps:
	git submodule update --init --recursive
	$(foreach dep,$(shell ls framework), cd $(BASE_DIR)/framework/$(dep) && git checkout master && git pull;)
	cd riak/riak && git checkout develop && git pull
	cd riak/riak_ee && git checkout develop && git pull

clean:
	$(foreach dep,$(shell ls framework), cd $(BASE_DIR)/framework/$(dep) && $(MAKE) clean && git reset --hard HEAD;)
	cd riak && $(MAKE) clean

updatehead:
	$(foreach dep,$(shell ls framework), cd $(BASE_DIR)/framework/$(dep) && git fetch origin $($(dep)_TAG) && git checkout $($(dep)_TAG);)
