#!/bin/bash
echo "This script installs and runs adblock2privoxy on CentOS 7 x64"

#c1.medium, ami-5fb8c835

echo "Find lates RPM"
latestRpm=$(ls ${INPUT1_STAGING_DIR}/*.el7.centos.x86_64.rpm|sort|tail -n 1)
echo "latest RPM is: $latestRpm"

echo "Install packages"
sudo yum-config-manager --enable epel
sudo yum install -y p7zip.x86_64
sudo rpm -i $latestRpm

for task in ${INPUT1_STAGING_DIR}/*.task 
do
    echo "process task $task"
    filename="${task##*/}"
    filename="${filename%.*}"
    adblock2privoxy -f -t $task -p privoxy -w web -d www.example.com
    echo "archive result"
    7za a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on ${OUTPUT1_STAGING_DIR}/ab2p.$filename.7z privoxy web $task
    echo "copy updated task to output"
    cp $task ${OUTPUT1_STAGING_DIR}
    echo "remove output"
    rm -rf privoxy web
done

echo "Lists processed at **`date +%d-%m-%Y`**" > ${OUTPUT1_STAGING_DIR}/update1.rst
echo "Done"
exit