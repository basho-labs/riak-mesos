# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"
Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.define 'ubuntu' do |node|
    node.vm.hostname = 'ubuntu.local'
    node.vm.network :private_network, ip: '192.168.42.42'
  end
  config.vm.box = "ubuntu/trusty64"
  config.vm.provider :virtualbox do |vb, override|
    vb.customize ["modifyvm", :id, "--memory", 8000,  "--cpus", "4"]
    override.vm.network :forwarded_port, guest: 5050, host: 5050 # Mesos Master
    override.vm.network :forwarded_port, guest: 5051, host: 5051 # Mesos Agent
    override.vm.network :forwarded_port, guest: 8080, host: 8080 # Marathon
    override.vm.network :forwarded_port, guest: 2181, host: 2181 # Zookeeper
    # override.vm.network :forwarded_port, guest: 9000, host: 9000 # Riak Explorer / Riak Director
    override.vm.network :forwarded_port, guest: 9090, host: 9090 # Riak Mesos Scheduler
    override.vm.network :forwarded_port, guest: 8098, host: 8098 # Riak HTTP
    override.vm.network :forwarded_port, guest: 8087, host: 8087 # Riak PB
    override.vm.network :forwarded_port, guest: 9999, host: 9999 # Reserved for testing
  end
  config.vm.provision 'shell', path: 'vagrant/provision.sh', run: 'once'
end
