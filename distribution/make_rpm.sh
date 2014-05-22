#!/bin/bash
set -e

echo "make build env"
./resolve_cabal_variables.sh rpmbuild/SPECS/template.spec > rpmbuild/SPECS/adblock2privoxy.spec

echo "install build tools and dependencies"
sudo yum -y install wget
sudo yum -y install @development-tools
sudo yum -y install fedora-packager
sudo yum-builddep -y rpmbuild/SPECS/adblock2privoxy.spec

echo "get source code"
wget -P rpmbuild/SOURCES/ "$(./resolve_cabal_variables.sh rpmbuild/SOURCES/download-source.url)"

echo "build"
rpmbuild --define "_topdir `pwd`/rpmbuild" -ba rpmbuild/SPECS/adblock2privoxy.spec