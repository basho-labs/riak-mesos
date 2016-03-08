# riak-mesos-erlang

Top-level application forming the erlang-based Mesos Framework for running Riak

## Development Build

```
env riak-mesos-scheduler_TAG=master \
    riak-mesos-executor_TAG=master \
    riak-mesos-director_TAG=master \
    riak_explorer_TAG=riak-addon-master \
    riak-mesos-tools_TAG=ack-refactor \
    riak-mesos-dcos-repo_TAG=ack-refactor \
    make
```

## Release Build

```
make
```
