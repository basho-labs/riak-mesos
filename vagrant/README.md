# Vagrant

Installs Mesos 0.26 + Erlang R16B02 + Marathon + ZK + Mesos-DNS on Ubuntu 14.04

# Usage

## Vagrant Up / Vagrant Reload

```
git clone https://github.com/basho-labs/riak-mesos-erlang.git
cd riak-mesos-erlang
vagrant up
vagrant reload # Required to complete installation of mesos-dns
```

## Verify Mesos DNS is running

```
vagrant ssh
curl http://master.mesos:5050
```

## Riak Mesos Erlang

First, you'll need to either:

1. Copy your github keys to ~/.ssh on the vagrant machine or
2. Generate a new github key to use within the vagrant env

Then run the following:

```
cd /vagrant
make dev
```

### Github SSH Notes:

Here are the steps I took to get Github keys set up:

1. From MacOS: `cd riak-mesos-erlang && cp ~/.ssh/id_rsa* ./`
2. From VM: `mkdir -p ~/.ssh && mv /vagrant/id_rsa* ~/.ssh/`
3. From VM: `echo 'eval "$(ssh-agent -s)"' >> ~/.bashrc && eval "$(ssh-agent -s)"`
4. From VM: `ssh-add` (And then enter your passphrase for the id_rsa)
5. From VM: `cd /vagrant && make dev`

At this point you should be able to successfully check out all of the required private repos.

**Note:** `ssh-add` needs to be run each time you start a new shell session for the VM.
