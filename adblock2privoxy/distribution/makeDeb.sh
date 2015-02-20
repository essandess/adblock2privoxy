#!/bin/bash
set -e
echo "This script builds adblock2privoxy to binary DEB on Debian 7."
echo "It can be easilly adapted to any other linux system with replacing pathes and apt-get calls to corresponding package manager"

initialDir=$PWD

echo "install tools"
sudo apt-get -y install libgmp10-dev libz-dev checkinstall

echo "install ghc"
cd $initialDir
wget https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.bz2
tar xvjf ghc-7.8.4-x86_64-unknown-linux-deb7.tar.bz2 
cd ghc-7.8.4

./configure --prefix=/home/admin/ghc
make install

export PATH=$PATH:/home/admin/.cabal/bin:/home/admin/ghc/bin

echo "install cabal"
cd $initialDir
wget http://hackage.haskell.org/package/cabal-install-1.20.0.3/cabal-install-1.20.0.3.tar.gz
tar -zxvf cabal-install-1.20.0.3.tar.gz
cd cabal-install-1.20.0.3
sh bootstrap.sh

echo "change dir"
script="$(readlink -f ${BASH_SOURCE[0]})"
scriptDir="$(dirname $script)"
cd $scriptDir
echo "working dir is $PWD"

echo "build adblock2privoxy"
cd ..
cabal update
cabal install --user --only-dependencies --enable-optimization=2
runhaskell Setup.hs configure --user --enable-optimization=2 --prefix=/usr
runhaskell Setup.hs build
runhaskell Setup.hs copy --destdir=distribution/debbuild
cp man distribution/debbuild/usr/share/

echo "create DEB"
mkdir -p $initialDir/result
dpkg-deb -b distribution/debbuild $initialDir/result
cd  $initialDir/result

echo "Build is done. The result is in current folder"