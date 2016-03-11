# riak-mesos-erlang

Top-level application forming the erlang-based Mesos Framework for running Riak

## Development Build

```
env riak-mesos-scheduler_TAG=master \
    riak-mesos-executor_TAG=master \
    riak-mesos-director_TAG=master \
    riak_explorer_TAG=riak-addon-master \
    riak-mesos-tools_TAG=master \
    riak-mesos-dcos-repo_TAG=master \
    make
```

Test with `riak-mesos-tools`:

```
make update-tools
cd tools/riak-mesos-tools
mkdir -p /etc/riak-mesos
cp config/config.local.json /etc/riak-mesos/config.json
make env
source env/bin/activate
riak-mesos framework install
riak-mesos cluster list
riak-mesos cluster create
```

## Release Build

```
make
```

## Outputs

Creates:

* `framework/*/packages/*.tar.gz`: Framework artifacts
* `framework/*/packages/*.tar.gz.sha`: Hashes
* `framework/*/packages/remote.txt`: Future remote package locations
* `framework/*/packages/local.txt`: Current local package locations
* `riak/packages/*.tar.gz`: Riak artifacts
* `riak/packages/*.tar.gz.sha`: Riak artifacts
* `riak/packages/remote.txt`: Future remote package location
* `riak/packages/local.txt`: Current local package location
