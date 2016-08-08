## Development environment

For quick and convenient development, a Vagrant-based development environment is provided.

To set it up, clone this repository and pull in all components from their git submodules:

```
 git clone https://github.com/basho-labs/riak-mesos
 cd riak-mesos
 git submodule update --init --recursive
 vagrant up
```

To log into the VM, now run:

```
vagrant ssh
```

You should now have Mesos and Marathon running on your VM, available as:

 - Mesos: http://ubuntu.local:5050/
 - Marathon: http://ubuntu.local:8080/

## Development Build

To build the full framework (including a copy of Riak-KV - see git submodules for exactly which version), run:

```
# Build all RMF packages locally
# This will take some time to complete as it builds each component
cd /vagrant/ && make dev
# Use a pre-made config file appropriate for this vagrant environment
mkdir -p /etc/riak-mesos
ln -nsf /vagrant/config/config.vagrant.github.json /etc/riak-mesos/config.json
```

Test with `riak-mesos-tools`:

```
cd /vagrant/
make env
source env/bin/activate
riak-mesos framework install
riak-mesos cluster list
riak-mesos cluster create
riak-mesos node add
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
