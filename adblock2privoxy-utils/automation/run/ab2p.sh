#!/bin/bash
echo "This script installs and runs adblock2privoxy on Fedora 20 x64"

echo "change dir"
initialDir=$PWD
script="$(readlink -f ${BASH_SOURCE[0]})"
scriptDir="$(dirname $script)"
cd $scriptDir
echo "working dir is $PWD"


echo "Install packages"
sudo yum install -y p7zip
sudo rpm -i adblock2privoxy*rpm

mkdir $initialDir/result

for task in *.task 
do
    echo "process $task"
    filename="${task%.*}"
    adblock2privoxy -t $task -p privoxy -w web -d www.example.com
    echo "archive result"
    7za a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on $initialDir/result/adblock2privoxy.$filename.7z privoxy web $task
    echo "remove output"
    rm -rf privoxy web $task
done

cd $initialDir/result

echo "Done. The result is in current folder"
exit