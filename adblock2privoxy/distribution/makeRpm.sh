#!/bin/bash
set -e
echo "This script compiles adblock2privoxy to binary RPM on Fedora Core 19-23, CentOS 6-7 x64"

echo "determine OS version"
if [ -e /etc/os-release ]
	then
		source /etc/os-release
	else
		VERSION_ID=$(cat /etc/centos-release | sed -r 's/[^0-9]+([0-9]).*/\1/')
		ID='centos'
fi

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

echo "setup repository"
curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/$ID/$VERSION_ID/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

echo "install build tools and dependencies"

if [ "$VERSION_ID" -gt "21" ]
	then
		sudo dnf -y install rpm-build
		sudo dnf builddep -y rpmbuild/SPECS/adblock2privoxy.spec
    else
		sudo yum -y install rpm-build
		sudo yum-builddep -y rpmbuild/SPECS/adblock2privoxy.spec
fi

echo "build RPM"
rpmbuild --define "_topdir `pwd`/rpmbuild" -bb rpmbuild/SPECS/adblock2privoxy.spec

echo "move RPM to result dir"
mkdir -p $initialDir/result
mv rpmbuild/RPMS/*/* $initialDir/result/
cd $initialDir/result

echo "Build is done."

read -t 15 -n 1 -p "Press any key to cancel shutdown"
if [ $? == 0 ]; then
    echo "The result is in current folder"
else
    sudo shutdown -h +10
fi
