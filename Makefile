BASE_DIR                       = $(PWD)
riak-mesos-scheduler_TAG      ?= 0.1.0
riak-mesos-executor_TAG       ?= 0.2.0
riak-mesos-director_TAG       ?= 0.3.1
riak_explorer_TAG             ?= 0.1.1-patch
riak-mesos-tools_TAG          ?= 0.4.0
riak-mesos-dcos-repo_TAG      ?= 0.4.0
export RIAK_TAG               ?= riak-2.1.3
export RIAK_SOURCE_DIR        ?= riak
export ARCH                   ?= amd64
export OSNAME                 ?= ubuntu
export OSVERSION              ?= trusty
riak-mesos-scheduler_BRANCH   ?= master
riak-mesos-executor_BRANCH    ?= master
riak-mesos-director_BRANCH    ?= master
riak_explorer_BRANCH          ?= riak-addon-master
riak-mesos-tools_BRANCH       ?= master
riak-mesos-dcos-repo_BRANCH   ?= master
TOOLS_TEMPLATE                ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.template.json
TOOLS_REMOTE                  ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.example.json
TOOLS_LOCAL                   ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.local.json
REPO_TEMPLATE                 ?= $(BASE_DIR)/tools/riak-mesos-dcos-repo/repo/packages/R/riak/0/config.template.json
REPO_REMOTE                   ?= $(BASE_DIR)/tools/riak-mesos-dcos-repo/repo/packages/R/riak/0/config.json
TOOLS_VERSION_FILE            ?= $(BASE_DIR)/tools/riak-mesos-tools/riak_mesos/constants.py
REPO_VERSION_FILE             ?= $(BASE_DIR)/tools/riak-mesos-dcos-repo/repo/packages/R/riak/0/package.json

.PHONY: all deps clean updatehead

all: deps update-head tarball config
dev: deps tarball config

.config.packages:
	cp $(TOOLS_TEMPLATE) $(TOOLS_REMOTE)
	cp $(TOOLS_TEMPLATE) $(TOOLS_LOCAL)
	cp $(REPO_TEMPLATE) $(REPO_REMOTE)
	sed -i "s,{{scheduler_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/remote.txt),g" $(TOOLS_REMOTE)
	sed -i "s,{{scheduler_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/local.txt),g" $(TOOLS_LOCAL)
	sed -i "s,{{scheduler_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/remote.txt),g" $(REPO_REMOTE)
	sed -i "s,{{executor_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/remote.txt),g" $(TOOLS_REMOTE)
	sed -i "s,{{executor_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/local.txt),g" $(TOOLS_LOCAL)
	sed -i "s,{{executor_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/remote.txt),g" $(REPO_REMOTE)
	sed -i "s,{{node_url}},$(shell cat $(BASE_DIR)/riak/packages/remote.txt),g" $(TOOLS_REMOTE)
	sed -i "s,{{node_url}},$(shell cat $(BASE_DIR)/riak/packages/local.txt),g" $(TOOLS_LOCAL)
	sed -i "s,{{node_url}},$(shell cat $(BASE_DIR)/riak/packages/remote.txt),g" $(REPO_REMOTE)
	sed -i "s,{{proxy_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/remote.txt),g" $(TOOLS_REMOTE)
	sed -i "s,{{proxy_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/local.txt),g" $(TOOLS_LOCAL)
	sed -i "s,{{proxy_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/remote.txt),g" $(REPO_REMOTE)
	sed -i "s,{{explorer_url}},$(shell cat $(BASE_DIR)/framework/riak_explorer/packages/remote.txt),g" $(TOOLS_REMOTE)
	sed -i "s,{{explorer_url}},$(shell cat $(BASE_DIR)/framework/riak_explorer/packages/local.txt),g" $(TOOLS_LOCAL)
	sed -i "s,{{explorer_url}},$(shell cat $(BASE_DIR)/framework/riak_explorer/packages/remote.txt),g" $(REPO_REMOTE)
	touch .config.packages
.config.version:
	sed -i "s,^version = .*$$,version = '$(riak-mesos-tools_TAG)',g" $(TOOLS_VERSION_FILE)
	sed -i "s,\"version\": \".*\",\"version\": \"$(riak-mesos-dcos-repo_TAG)\",g" $(REPO_VERSION_FILE)
	cd tools/riak-mesos-dcos-repo/scripts && \
			./0-validate-version.sh && \
			./1-validate-packages.sh && \
			./2-build-index.sh && \
			./3-validate-index.sh
	touch .config.version
config: .config.packages .config.version

.tarball.framework:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && $(MAKE) tarball;)
	touch .tarball.framework
.tarball.riak:
	cd riak && $(MAKE) tarball
	touch .tarball.riak
tarball: .tarball.framework .tarball.riak

.deps.submodules:
	git submodule update --init --recursive
	touch .deps.submodules
.deps.framework:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && \
			git checkout -q $($(dep)_BRANCH) && git pull -q;)
	touch .deps.framework
.deps.tools:
	$(foreach dep,$(shell ls tools), \
		cd $(BASE_DIR)/tools/$(dep) && \
			git checkout -q $($(dep)_BRANCH) && git pull -q;)
	touch .deps.tools
.deps.riak:
	cd $(BASE_DIR)/riak/$(RIAK_SOURCE_DIR) && \
			git checkout -q develop && git pull -q && \
			git checkout -q $(RIAK_TAG)
	touch .deps.riak
deps: .deps.submodules .deps.framework .deps.tools .deps.riak

clean-config:
	-rm .config.*
clean-submodules:
	-rm .deps.submodules
clean-framework:
	-rm .tarball.framework .deps.framework
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && \
			$(MAKE) clean && rm -rf deps/* && rm -rf ebin/*.beam && git reset --hard HEAD;)
clean-tools:
	-rm .deps.framework
clean-riak:
	-rm .tarball.riak .deps.riak
	-rm -rf riak/$(RIAK_SOURCE_DIR)/deps/*
	cd riak && $(MAKE) clean && 
clean: clean-config clean-submodules clean-framework clean-tools clean-riak

update-head:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && \
			git fetch -q origin $($(dep)_TAG) && git checkout -q $($(dep)_TAG);)
	$(foreach dep,$(shell ls tools), \
		cd $(BASE_DIR)/tools/$(dep) && \
			git fetch -q origin $($(dep)_TAG) && git checkout -q $($(dep)_TAG);)
	cd $(BASE_DIR)/riak/$(RIAK_SOURCE_DIR) && \
		git fetch -q origin $(RIAK_TAG) && git checkout -q $(RIAK_TAG)

sync:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && $(MAKE) sync;)
	cd $(BASE_DIR)/riak && make sync
