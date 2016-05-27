BASE_DIR                       = $(PWD)
riak-mesos-scheduler_TAG      ?= 1.0-rc2
riak-mesos-executor_TAG       ?= 1.0-rc2
riak-mesos-director_TAG       ?= 1.0-rc1
riak_explorer_TAG             ?= 0.3.0
riak-mesos-tools_TAG          ?= 1.0-rc2
riak-mesos-dcos-repo_TAG      ?= 1.0-rc2
export RIAK_TAG               ?= riak-2.1.3
export RIAK_SOURCE_DIR        ?= riak
export ARCH                   ?= amd64
export OS_FAMILY                 ?= ubuntu
export OS_VERSION             ?= 14.04
riak-mesos-scheduler_BRANCH   ?= master
riak-mesos-executor_BRANCH    ?= master
riak-mesos-director_BRANCH    ?= master
riak_explorer_BRANCH          ?= master
riak-mesos-tools_BRANCH       ?= master
riak-mesos-dcos-repo_BRANCH   ?= master
DCOS_TEMPLATE                 ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.dcos.template.json
DCOS_REMOTE										?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.dcos.json
TOOLS_TEMPLATE                ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.template.json
TOOLS_REMOTE                  ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.example.json
TOOLS_LOCAL                   ?= $(BASE_DIR)/tools/riak-mesos-tools/config/config.local.json
REPO_TEMPLATE                 ?= $(BASE_DIR)/tools/riak-mesos-dcos-repo/repo/packages/R/riak/0/config.template.json
REPO_REMOTE                   ?= $(BASE_DIR)/tools/riak-mesos-dcos-repo/repo/packages/R/riak/0/config.json
TOOLS_VERSION_FILE            ?= $(BASE_DIR)/tools/riak-mesos-tools/riak_mesos/constants.py
REPO_VERSION_FILE             ?= $(BASE_DIR)/tools/riak-mesos-dcos-repo/repo/packages/R/riak/0/package.json
REPO_CMD_TEMPLATE             ?= $(BASE_DIR)/tools/riak-mesos-dcos-repo/repo/packages/R/riak/0/command.template.json
REPO_CMD_FILE                 ?= $(BASE_DIR)/tools/riak-mesos-dcos-repo/repo/packages/R/riak/0/command.json

.PHONY: all deps clean update-head

all: deps update-head tarball config
dev: deps tarball config

.config.packages:
	cp $(TOOLS_TEMPLATE) $(TOOLS_REMOTE) && \
	cp $(TOOLS_TEMPLATE) $(TOOLS_LOCAL) && \
	cp $(REPO_TEMPLATE) $(REPO_REMOTE) && \
	cp $(DCOS_TEMPLATE) $(DCOS_REMOTE) && \
	sed -i "s,{{scheduler_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{scheduler_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{scheduler_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/remote.txt),g" $(REPO_REMOTE) && \
	sed -i "s,{{scheduler_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/remote.txt),g" $(DCOS_REMOTE) && \
	sed -i "s,{{executor_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{executor_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{executor_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/remote.txt),g" $(REPO_REMOTE) && \
	sed -i "s,{{executor_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/remote.txt),g" $(DCOS_REMOTE) && \
	sed -i "s,{{node_url}},$(shell cat $(BASE_DIR)/riak/packages/remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{node_url}},$(shell cat $(BASE_DIR)/riak/packages/local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{node_url}},$(shell cat $(BASE_DIR)/riak/packages/remote.txt),g" $(REPO_REMOTE) && \
	sed -i "s,{{node_url}},$(shell cat $(BASE_DIR)/riak/packages/remote.txt),g" $(DCOS_REMOTE) && \
	sed -i "s,{{director_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{director_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{director_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/remote.txt),g" $(REPO_REMOTE) && \
	sed -i "s,{{director_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-director/packages/remote.txt),g" $(DCOS_REMOTE) && \
	sed -i "s,{{explorer_url}},$(shell cat $(BASE_DIR)/framework/riak_explorer/packages/remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{explorer_url}},$(shell cat $(BASE_DIR)/framework/riak_explorer/packages/local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{explorer_url}},$(shell cat $(BASE_DIR)/framework/riak_explorer/packages/remote.txt),g" $(REPO_REMOTE) && \
	sed -i "s,{{explorer_url}},$(shell cat $(BASE_DIR)/framework/riak_explorer/packages/remote.txt),g" $(DCOS_REMOTE) && \
	sed -i "s,{{patches_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/patches_remote.txt),g" $(TOOLS_REMOTE) && \
	sed -i "s,{{patches_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/patches_local.txt),g" $(TOOLS_LOCAL) && \
	sed -i "s,{{patches_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/patches_remote.txt),g" $(REPO_REMOTE) && \
	sed -i "s,{{patches_url}},$(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/patches_remote.txt),g" $(DCOS_REMOTE) && \
	sed -i "s,{{patches_package}},$(shell basename $(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/patches_local.txt)),g" $(REPO_REMOTE) && \
	sed -i "s,{{patches_package}},$(shell basename $(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/patches_local.txt)),g" $(DCOS_REMOTE) && \
	sed -i "s,{{explorer_package}},$(shell basename $(shell cat $(BASE_DIR)/framework/riak_explorer/packages/local.txt)),g" $(REPO_REMOTE) && \
	sed -i "s,{{explorer_package}},$(shell basename $(shell cat $(BASE_DIR)/framework/riak_explorer/packages/local.txt)),g" $(DCOS_REMOTE) && \
	sed -i "s,{{node_package}},$(shell basename $(shell cat $(BASE_DIR)/riak/packages/local.txt)),g" $(REPO_REMOTE) && \
	sed -i "s,{{node_package}},$(shell basename $(shell cat $(BASE_DIR)/riak/packages/local.txt)),g" $(DCOS_REMOTE) && \
	sed -i "s,{{executor_package}},$(shell basename $(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/local.txt)),g" $(REPO_REMOTE) && \
	sed -i "s,{{executor_package}},$(shell basename $(shell cat $(BASE_DIR)/framework/riak-mesos-executor/packages/local.txt)),g" $(DCOS_REMOTE) && \
	sed -i "s,{{scheduler_package}},$(shell basename $(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/local.txt)),g" $(REPO_REMOTE) && \
	sed -i "s,{{scheduler_package}},$(shell basename $(shell cat $(BASE_DIR)/framework/riak-mesos-scheduler/packages/local.txt)),g" $(DCOS_REMOTE)
.config.version:
	cp $(REPO_CMD_TEMPLATE) $(REPO_CMD_FILE) && \
	sed -i "s,^version = .*$$,version = '$(riak-mesos-tools_TAG)',g" $(TOOLS_VERSION_FILE) && \
	sed -i "s,\"version\": \".*\",\"version\": \"$(riak-mesos-dcos-repo_TAG)\",g" $(REPO_VERSION_FILE) && \
	sed -i "s,{{tools_version}},$(riak-mesos-tools_TAG),g" $(REPO_CMD_FILE) && \
	cd tools/riak-mesos-dcos-repo/scripts && \
			./0-validate-version.sh && \
			./1-validate-packages.sh && \
			./2-build-index.sh && \
			./3-validate-index.sh
config: .config.packages .config.version

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
.tarball.riak:
	cd $(BASE_DIR)/riak && $(MAKE) tarball && \
	touch ../.tarball.riak
tarball: .tarball.framework .tarball.riak

.deps.submodules:
	git submodule update --init --recursive
.deps.framework:
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && \
			git checkout -q $($(dep)_BRANCH) && git pull -q;)
.deps.tools:
	$(foreach dep,$(shell ls tools), \
		cd $(BASE_DIR)/tools/$(dep) && \
			git checkout -q $($(dep)_BRANCH) && git pull -q;)
.deps.riak:
	cd $(BASE_DIR)/riak/$(RIAK_SOURCE_DIR) && \
			git checkout -q develop && git pull -q && \
			git checkout -q $(RIAK_TAG)
deps: .deps.submodules .deps.framework .deps.tools .deps.riak

clean-framework:
	-rm .tarball.riak-mesos-scheduler .tarball.riak-mesos-executor .tarball.riak-mesos-director .tarball.riak_explorer
	$(foreach dep,$(shell ls framework), \
		cd $(BASE_DIR)/framework/$(dep) && \
			$(MAKE) clean && rm -rf deps/* && rm -rf ebin/*.beam && git reset --hard HEAD;)
clean-riak:
	-rm .tarball.riak
	-rm -rf riak/$(RIAK_SOURCE_DIR)/deps/*
	cd riak && $(MAKE) clean
clean: clean-framework

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
