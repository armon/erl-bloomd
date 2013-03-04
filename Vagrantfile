$script = <<EOF
set -e

# Install the things we need to build
apt-get update
apt-get install -y autoconf
apt-get install -y automake
apt-get install -y build-essential
apt-get install -y git-core
apt-get install -y libtool
apt-get install -y telnet
apt-get install -y scons

# Compile Erlang from source
if [ ! -f /usr/local/bin/erl ]; then
  pushd /tmp

  # Download Erlang
  wget --progress=dot -e dotbytes=1M http://www.erlang.org/download/otp_src_R15B03-1.tar.gz

  # Untar it
  tar xvzf otp_src_R15B03-1.tar.gz

  # Compile it
  pushd otp_src_R15B03
  ./configure
  make
  make install
  popd

  popd
fi

if [ -d bloomd ]; then
  cd bloomd && git pull && cd ..
else
  git clone --depth 1 https://github.com/armon/bloomd
fi

cd bloomd
scons
mv bloomd /usr/local/bin/bloomd
cd ..
echo > /home/vagrant/bloomd.conf <<EOE
[bloomd]
data_dir=/home/vagrant
flush_interval=60000
port=8673
workers=1
log_level=debug"
EOE

ps elf | grep -i bloomd | awk '{print "kill -9 "$2}' |sh

bloomd -f /home/vagrant/bloomd.conf &
echo "Done."
EOF

Vagrant.configure("2") do |config|
  config.vm.box = "precise64"
  config.vm.provision :shell, :inline => $script
  config.vm.network :hostonly,
    ip: "33.33.38.10"
end
