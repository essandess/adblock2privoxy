#!/bin/bash
echo "prepare build:"
echo "remove temp files"
rm -rf ../../adblock2privoxy/.dist-buildwrapper
rm -rf ../../adblock2privoxy/.stack-work
rm -rf ../../adblock2privoxy/dist

echo "start build (see */build.log files for details)"
parallel --max-procs 0 ::: \
'./run_aws_build.sh ami-e0efab88 admin  "../../adblock2privoxy" "distribution/makeDeb" debian7_64' \
'./run_aws_build.sh ami-8b9a63e0 admin  "../../adblock2privoxy" "distribution/makeDeb" debian8_64' \
'./run_aws_build.sh ami-62adbc0a fedora "../../adblock2privoxy" "distribution/makeRpm" fedora22_64' \
'./run_aws_build.sh ami-02321068 fedora "../../adblock2privoxy" "distribution/makeRpm" fedora23_64' \
'./run_aws_build.sh ami-02dc4c6b ec2-user "../../adblock2privoxy" "distribution/makeRpm" centos6_64' \
'./run_aws_build.sh ami-61bbf104 centos "../../adblock2privoxy" "distribution/makeRpm" centos7_64'
