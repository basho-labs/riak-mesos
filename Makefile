BASE_DIR                       = $(PWD)
WGET                          ?= $(shell which wget)
TOOLS_TEMPLATE                ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.template.json
TOOLS_REMOTE                  ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.example.json
TOOLS_LOCAL                   ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.local.json
PKG_CACHE_DIR                 ?= $(BASE_DIR)/packages
RIAK_KV_REMOTE                ?= "https://github.com/basho-labs/riak-mesos/releases/download/2.0.0/riak-2.2.0-ubuntu-14.04.tar.gz"
RIAK_TS_REMOTE                ?= "https://github.com/basho-labs/riak-mesos/releases/download/2.0.0/riak_ts-1.5.1-ubuntu-14.04.tar.gz"
RIAK_KV_LOCAL                 ?= $(PKG_CACHE_DIR)/riak-2.2.0-ubuntu-14.04.tar.gz
RIAK_TS_LOCAL                 ?= $(PKG_CACHE_DIR)/riak_ts-1.5.1-ubuntu-14.04.tar.gz

.PHONY: all deps clean update-head cache-packages

all: update-head tarball config
dev: deps tarball config

cache-packages:
	mkdir -p $(PKG_CACHE_DIR)
	$(WGET) $(RIAK_KV_REMOTE) -O $(RIAK_KV_LOCAL)
	$(WGET) $(RIAK_TS_REMOTE) -O $(RIAK_TS_LOCAL)

.config.packages:
	cp $(TOOLS_TEMPLATE) $(TOOLS_REMOTE) && \
	cp $(TOOLS_TEMPLATE) $(TOOLS_LOCAL) && \
	sed -i "s,{{scheduler_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{scheduler_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{executor_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{executor_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{patches_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/patches_remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{patches_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/patches_local.txt),g" $(TOOLS_LOCAL)
	sed -i "s,{{explorer_url}},$(shell cat $(BASE_DIR)/framework/riak_explorer/packages/remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{explorer_url}},$(shell cat $(BASE_DIR)/framework/riak_explorer/packages/local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{director_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{director_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{riak_kv_2_2_url}},$(RIAK_KV_REMOTE),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{riak_kv_2_2_url}},$(RIAK_KV_LOCAL),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{riak_ts_1_5_url}},$(RIAK_TS_REMOTE),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{riak_ts_1_5_url}},$(RIAK_TS_LOCAL),g" $(TOOLS_LOCAL)
.config.version:
	cd tools/riak-mesos-dcos-repo/scripts && ./build.sh
config: cache-packages .config.packages .config.version

.tarball.riak-mesos-scheduler:
	cd $(BASE_DIR)/framework/riak-mesos-scheduler && $(MAKE) tarball && \
	touch ../../.tarball.riak-mesos-scheduler
.tarball.riak-mesos-executor:
	cd $(BASE_DIR)/framework/riak-mesos-executor && $(MAKE) tarball && \
	touch ../../.tarball.riak-mesos-executor
.tarball.riak-mesos-director:
	cd $(BASE_DIR)/framework/riak-mesos-director && $(MAKE) tarball && \
	touch ../../.tarball.riak-mesos-director
.tarball.riak_explorer:
	cd $(BASE_DIR)/framework/riak_explorer && $(MAKE) tarball && \
	touch ../../.tarball.riak_explorer
.tarball.framework: .tarball.riak-mesos-scheduler .tarball.riak-mesos-executor .tarball.riak-mesos-director .tarball.riak_explorer
tarball: .tarball.framework

deps:
	@if [ -z "$$(git submodule foreach ls)" ]; then \
		git submodule update --init --recursive; \
	fi

clean-framework:
	-rm .tarball.riak-mesos-scheduler .tarball.riak-mesos-executor .tarball.riak-mesos-director .tarball.riak_explorer
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && \
			$(MAKE) clean && rm -rf deps/* && rm -rf ebin/*.beam && git reset --hard HEAD;)
clean: clean-framework

update-head:
	git submodule update --init --recursive

sync:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && $(MAKE) sync;)

