BASE_DIR                    = $(PWD)
riak-mesos-scheduler_TAG   ?= 0.0.1
riak-mesos-executor_TAG    ?= 0.1.2
riak-mesos-director_TAG    ?= 0.3.1
riak_explorer_TAG          ?= 0.1.1-patch
riak-mesos-tools_TAG       ?= 0.3.1
riak-mesos-dcos-repo_TAG   ?= 0.3.1
export RIAK_TAG            ?= riak-2.1.3
export RIAK_SOURCE_DIR     ?= riak
export ARCH                ?= amd64
export OSNAME              ?= ubuntu
export OSVERSION           ?= trusty

.PHONY: all deps clean updatehead

all: deps updatehead tarball

tarball:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && $(MAKE) tarball;)
	cd riak && $(MAKE) tarball

update-tools:
	SCHEDULER_URL_REMOTE = $(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/remote.txt)
	SCHEDULER_URL_LOCAL = $(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/local.txt)
	EXECUTOR_URL_REMOTE = $(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/remote.txt)
	EXECUTOR_URL_LOCAL = $(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/local.txt)
	NODE_URL_REMOTE = $(shell cat $(BASE_DIR)/riak/packages/remote.txt)
	NODE_URL_LOCAL = $(shell cat $(BASE_DIR)/riak/packages/local.txt)
	PROXY_URL_REMOTE = $(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/remote.txt)
	PROXY_URL_LOCAL = $(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/local.txt)
	cp $(BASE_DIR)/tools/riak-mesos-tools/config/config.template.json $(BASE_DIR)/tools/riak-mesos-tools/config/config.example.json
	cp $(BASE_DIR)/tools/riak-mesos-tools/config/config.template.json $(BASE_DIR)/tools/riak-mesos-tools/config/config.local.json
	$(shell sed -i '' "s,{{scheduler_url}},$(SCHEDULER_URL_REMOTE),g" $(BASE_DIR)/tools/riak-mesos-tools/config/config.example.json)
	$(shell sed -i '' "s,{{scheduler_url}},$(SCHEDULER_URL_LOCAL),g" $(BASE_DIR)/tools/riak-mesos-tools/config/config.local.json)
	$(shell sed -i '' "s,{{executor_url}},$(EXECUTOR_URL_REMOTE),g" $(BASE_DIR)/tools/riak-mesos-tools/config/config.example.json)
	$(shell sed -i '' "s,{{executor_url}},$(EXECUTOR_URL_LOCAL),g" $(BASE_DIR)/tools/riak-mesos-tools/config/config.local.json)
	$(shell sed -i '' "s,{{node_url}},$(NODE_URL_REMOTE),g" $(BASE_DIR)/tools/riak-mesos-tools/config/config.example.json)
	$(shell sed -i '' "s,{{node_url}},$(NODE_URL_LOCAL),g" $(BASE_DIR)/tools/riak-mesos-tools/config/config.local.json)
	$(shell sed -i '' "s,{{proxy_url}},$(PROXY_URL_REMOTE),g" $(BASE_DIR)/tools/riak-mesos-tools/config/config.example.json)
	$(shell sed -i '' "s,{{proxy_url}},$(PROXY_URL_LOCAL),g" $(BASE_DIR)/tools/riak-mesos-tools/config/config.local.json)
	$(shell sed -i '' "s,^version = .*$$,version = '$(riak-mesos-tools_TAG)',g" $(BASE_DIR)/tools/riak-mesos-tools/riak_mesos/constants.py)

deps:
	git submodule update --init --recursive
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && \
			git checkout master && git pull;)
	$(foreach dep,$(shell ls tools), \
		cd $(BASE_DIR)/tools/$(dep) && \
			git checkout master && git pull;)
	cd $(BASE_DIR)/riak/$(RIAK_SOURCE_DIR) && git checkout develop && git pull

clean:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && \
			$(MAKE) clean && git reset --hard HEAD;)
	cd riak && $(MAKE) clean

updatehead:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && \
			git fetch origin $($(dep)_TAG) && git checkout $($(dep)_TAG);)
	$(foreach dep,$(shell ls tools), \
		cd $(BASE_DIR)/tools/$(dep) && \
			git fetch origin $($(dep)_TAG) && git checkout $($(dep)_TAG);)
	cd $(BASE_DIR)/riak/$(RIAK_SOURCE_DIR) && \
		git fetch origin $(RIAK_TAG) && git checkout $(RIAK_TAG)

sync:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && $(MAKE) sync;)
	cd $(BASE_DIR)/riak && make sync
