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

# tools
apt-get -y install jq ngrep apt-show-versions

# framework
apt-get -y install zookeeper=3.4.5+dfsg-1
apt-get -y install marathon=1.3.6-1.0.540.ubuntu1404
#apt-get -y install mesos=0.26.0-0.2.145.ubuntu1404
apt-get -y install mesos=1.0.1-2.0.93.ubuntu1404


# apt-get -y install mesos # 1.0.0
apt-mark hold mesos marathon zookeeper
apt-get -y install lxc-docker
apt-get -y install resolvconf

# Configure mesos
# Only need this for mesos < 0.27
#echo "riak" > /etc/mesos-master/roles
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
pip install mesos.cli
# NB there's a transitive dep of protobuf and mesos.cli that causes issues if you install
# protobuf==2.6.1 first...
pip install protobuf==2.6.1

# Install kerl, Erlang
apt-get -y install libncurses5-dev libpam0g-dev
apt-get install -y build-essential autoconf libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev libpam0g-dev maven

mkdir -p $HOME/bin
export PATH="$PATH:$HOME/bin"
cd $HOME/bin

# BEGIN Kerl setup
KERL_VSN="1.1.1"
KERL_DIR="$HOME/bin/kerl-$KERL_VSN"
TARGET_ERL=R16B02_basho10
[ ! -d "kerl-$KERL_VSN" ] && (
    curl -L -O https://github.com/kerl/kerl/archive/$KERL_VSN.tar.gz
    tar zxf $KERL_VSN.tar.gz && rm $KERL_VSN.tar.gz
    [ -f "$KERL_DIR/kerl" ] && ln -nsf "$KERL_DIR/kerl" "$HOME/bin/kerl"
    )

# TODO Add KERL_BUILD_DOCS=yes KERL_INSTALL_MANPAGES=yes
# Once the bug for non-standard version nrs in kerl 1.1.1 is snuffed out
[ ! -f "$HOME/.kerlrc" ] && cat > $HOME/.kerlrc << _KERLRC
export KERL_BUILD_BACKEND=git
export OTP_GITHUB_URL="https://github.com/basho/otp"
_KERLRC

# Only attempt to run kerl if it's in $PATH
[ $(which kerl) ] && (
    # kerl 1.1.1 has a bug where OTP_GITHUB_URL doesn't get picked up from .kerlrc
    export OTP_GITHUB_URL="https://github.com/basho/otp"
    kerl update releases
    [ $(kerl list builds | grep -c "$TARGET_ERL") -eq 0 ] && kerl build "$TARGET_ERL" "$TARGET_ERL"
    [ $(kerl list installations | grep -c "$TARGET_ERL") -eq 0 ] && kerl install "$TARGET_ERL" "$HOME/erlang/$TARGET_ERL"
    ) || echo "ERROR: kerl was not installed"

[ $(grep -c '#kerl_completion' "$HOME/.bashrc") -eq 0 ] && (
    echo '#kerl_completion' >> "$HOME/.bashrc"
    echo "[ -f \"$KERL_DIR/bash_completion/kerl\" ] && . \"$KERL_DIR/bash_completion/kerl\"" >> $HOME/.bashrc
)

[ $(grep -c '#Erlang_activate' "$HOME/.bashrc") -eq 0 ] && (
    echo "#Erlang_activate" >> "$HOME/.bashrc"
    echo ". \$HOME/erlang/$TARGET_ERL/activate" >> "$HOME/.bashrc"
    echo 'export PATH=$PATH:$HOME/bin' >> "$HOME/.bashrc"
)

[ -f "$HOME/erlang/$TARGET_ERL/activate" ] && . "$HOME/erlang/$TARGET_ERL/activate"
# END Kerl setup

# Fix permissions
chown -R vagrant:vagrant $HOME

# Launch Mesos DNS
curl -v -XPUT -H 'Content-Type: application/json' http://localhost:8080/v2/apps -d @/vagrant/vagrant/mesos-dns-marathon.json

# Install riak-mesos-tools as 'riak-mesos'
# NB To reinstall after making changes to /vagrant/tools/riak-mesos-tools, use:
# $ sudo pip install --upgrade git+file:///vagrant/tools/riak-mesos-tools#egg=riak_mesos
# You will need to commit your changes locally.
sudo pip install git+file:///vagrant/tools/riak-mesos-tools#egg=riak_mesos
# TODO It would be nice to do this without sudo - install to somewhere inside $HOME/bin/ perhaps?
