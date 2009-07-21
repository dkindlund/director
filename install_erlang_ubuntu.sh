#!/bin/sh

ERLANG_SRC=otp_src_R12B-5.tar.gz

echo "=> Installing Erlang"

echo "=> Creating erlang_src folder under home directory"
mkdir -p ~/erlang_src
cd ~/erlang_src

echo "=> Updating Ubuntu"
sudo apt-get update

echo "=> Installing Build Essentials"
sudo apt-get install build-essential -y

echo "=> Installing wget"
sudo apt-get install wget -y

echo "=> Install Erlang deps"
sudo apt-get install libc6 libncurses5 libncurses5-dev libssl-dev openssl m4 libexpat1-dev -y

echo "=> Getting Erlang from erlang.org"
wget http://www.erlang.org/download/otp_src_R12B-5.tar.gz

echo "=> Extracting and building with make"
tar xzvf otp_src_R12B-5.tar.gz
cd otp_src_R12B-5
./configure
make

echo "Installing"
sudo make install

echo "Done!"