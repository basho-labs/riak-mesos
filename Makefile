BASE_DIR                    = $(PWD)
riak-mesos-scheduler_TAG   ?= master
riak-mesos-executor_TAG    ?= 0.1.1
riak-mesos-director_TAG    ?= 0.3.0
riak_explorer_TAG          ?= riak-addon-0.1.0

.PHONY: all deps clean updatehead

all: deps

deps:
	git submodule update --init --recursive

clean:
	$(foreach dep,$(wildcard deps/*), $(MAKE) clean;)

updatehead:
	git submodule update --init --recursive
	$(foreach dep,$(shell ls deps), cd $(BASE_DIR)/deps/$(dep) && git fetch origin $($(dep)_TAG) && git checkout origin/$($(dep)_TAG);)
