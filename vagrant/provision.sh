export DEBIAN_FRONTEND=noninteractive
export HOME=/home/vagrant

# Install mesos, marathon, zk, docker
apt-key adv --keyserver keyserver.ubuntu.com --recv E56151BF
DISTRO=$(lsb_release -is | tr '[:upper:]' '[:lower:]')
CODENAME=$(lsb_release -cs)
echo "deb http://repos.mesosphere.io/${DISTRO} ${CODENAME} main" |  sudo tee /etc/apt/sources.list.d/mesosphere.list
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 36A1D7869245C8950F966E92D8576A8BA88D21E9
echo "deb https://get.docker.io/ubuntu docker main" | sudo tee /etc/apt/sources.list.d/docker.list
apt-get -y update > /dev/null
apt-get -y upgrade > /dev/null
apt-get -y install software-properties-common
add-apt-repository -y ppa:openjdk-r/ppa
apt-get -y update > /dev/null
apt-get -y install make git gcc g++ curl
apt-get -y install python-dev libcppunit-dev libunwind8-dev autoconf autotools-dev libltdl-dev libtool autopoint libcurl4-openssl-dev libsasl2-dev
apt-get -y install openjdk-8-jdk default-jre python-setuptools python-protobuf
update-java-alternatives -s /usr/lib/jvm/java-1.8.0-openjdk-amd64
apt-get -y install libprotobuf-dev protobuf-compiler
apt-get -y install marathon
apt-get -y install mesos=0.26.0-0.2.145.ubuntu1404
apt-get -y install lxc-docker
apt-get -y install resolvconf

# Configure mesos
echo "riak" > /etc/mesos-master/roles
echo "1" > /etc/mesos-master/quorum
echo "/var/lib/mesos" > /etc/mesos-master/work_dir

# Mesos DNS
mkdir -p /usr/local/mesos-dns/
wget https://github.com/mesosphere/mesos-dns/releases/download/v0.5.1/mesos-dns-v0.5.1-linux-amd64
mv mesos-dns-v0.5.1-linux-amd64 /usr/local/mesos-dns/mesos-dns
chmod 755 /usr/local/mesos-dns/mesos-dns
# Configure Resolvconf
echo "nameserver 10.0.2.15" > /etc/resolvconf/resolv.conf.d/head
resolvconf -u

service zookeeper restart
service mesos-slave restart
service mesos-master restart
service marathon restart

apt-get -y install git s3cmd zip python-pip
pip install jsonschema
pip install virtualenv

# Install Erlang
apt-get -y install libncurses5-dev libpam0g-dev
apt-get install -y build-essential autoconf libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev libpam0g-dev maven
mkdir -p $HOME/bin
cd $HOME/bin
curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
chmod a+x kerl
./kerl build git git://github.com/basho/otp.git OTP_R16B02_basho8 R16B02-basho8
./kerl install R16B02-basho8 ~/erlang/R16B02-basho8
. ~/erlang/R16B02-basho8/activate

echo '# Erlang' >> $HOME/.bashrc
echo '. $HOME/erlang/R16B02-basho8/activate' >> $HOME/.bashrc
echo 'export PATH=$PATH:$HOME/bin' >> $HOME/.bashrc

# Fix permissions
chown -R vagrant:vagrant $HOME

# Launch Mesos DNS
curl -v -XPUT -H 'Content-Type: application/json' http://localhost:8080/v2/apps -d @/vagrant/vagrant/mesos-dns-marathon.json
