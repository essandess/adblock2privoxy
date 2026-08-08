#!/bin/bash
set -e
echo "This script builds adblock2privoxy to binary DEB on Debian 7-8 x64"
echo "It can be easilly adapted to any other linux system with replacing pathes and apt-get calls to corresponding package manager"

echo "remember dirs"
initialDir=$PWD
script="$(readlink -f ${BASH_SOURCE[0]})"
scriptDir="$(dirname $script)"
codename="$(lsb_release -c | sed -r 's/.+:\s+(\w+)/\1/')"

echo "install tools"
echo "deb http://download.fpcomplete.com/debian $codename main" | sudo tee /etc/apt/sources.list.d/fpco.list

sudo apt-get update
sudo apt-get -y --force-yes install stack libpcre2-dev

echo "install ghc and cabal"
cd ~
stack setup

echo "change dir to $scriptDir"
cd $scriptDir
echo "working dir is $PWD"

echo "build adblock2privoxy"
cd ..
stack build
stack install --local-bin-path=distribution/debbuild/usr/bin
cp -r man distribution/debbuild/usr/share/
echo "set architecture"
sed -i -e "s/#ARCH#/$(dpkg --print-architecture)/" distribution/debbuild/DEBIAN/control

echo "create DEB"
mkdir -p $initialDir/result
dpkg-deb -b distribution/debbuild $initialDir/result
cd  $initialDir/result
echo "rename result"
version=$(lsb_release -sr)
find . -name '*.deb' -exec sh -c 'mv "$0" "${0%.deb}.debian_$version.deb"' {} \;

echo "Build is done."

read -t 15 -n 1 -p "Press any key to cancel shutdown"
if [ $? == 0 ]; then
    echo "The result is in current folder"
else
    sudo shutdown -h +10
fi
