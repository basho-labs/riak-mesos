# Vagrant

Installs Mesos 0.26 + Erlang R16B02 + Marathon + ZK + Mesos-DNS on Ubuntu 14.04

# Usage

```
git clone https://github.com/basho-labs/riak-mesos-erlang.git
cd riak-mesos-erlang
vagrant up
vagrant reload # Required to complete installation of mesos-dns
```

Verify everything works as expected

```
vagrant ssh
curl http://master.mesos:5050
```
