#!/bin/bash
set -e
echo "This script compiles adblock2privoxy to binary RPM on Fedora Core 19-20"

echo "change dir"
initialDir=$PWD
script="$(readlink -f ${BASH_SOURCE[0]})"
scriptDir="$(dirname $script)"
cd $scriptDir
echo "working dir is $PWD"

echo "create build folders"
mkdir -p rpmbuild/BUILD
ln -nsf ../../.. rpmbuild/BUILD/root
mkdir -p rpmbuild/BUILDROOT
mkdir -p rpmbuild/RPMS

echo "install build tools and dependencies"
sudo yum -y install compat-libffi
sudo yum -y install @development-tools
sudo yum -y install fedora-packager
#sudo yum -y install alien
#sudo yum -y install dpkg-dev
sudo yum-builddep -y rpmbuild/SPECS/adblock2privoxy.spec

echo "build RPM"
rpmbuild --define "_topdir `pwd`/rpmbuild" -bb rpmbuild/SPECS/adblock2privoxy.spec

echo "move RPM to result dir"
mkdir -p $initialDir/result
mv rpmbuild/RPMS/*/* $initialDir/result/
cd $initialDir/result

echo "Build is done. The result is in current folder"
