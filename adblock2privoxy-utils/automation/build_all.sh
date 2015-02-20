#!/bin/bash
echo "prepare rpm build:"
echo "remove temp files"
rm -rf ../../adblock2privoxy/.dist-buildwrapper

echo "start RPM build (see fedora*/build.log files for details)"
parallel ::: \
'./run_aws_build.sh ami-1eb35469 fedora "../../adblock2privoxy" "distribution/makeFedoraRpm" fedora20_64' \
'./run_aws_build.sh ami-0bac577c fedora "../../adblock2privoxy" "distribution/makeFedoraRpm" fedora20_i386' 

echo "prepare deb build:"
echo "copy build script to bin directory"
cp ../../adblock2privoxy/distribution/makeDebFromRpm.sh fedora20_i386/
cp ../../adblock2privoxy/distribution/makeDebFromRpm.sh fedora20_64/

echo "start DEB build (see debian*/build.log files for details)"
parallel ::: \
'./run_aws_build.sh ami-e7e66a90 admin "fedora20_64" "makeDebFromRpm" debian7_64' \
'./run_aws_build.sh ami-1be06c6c admin "fedora20_i386" "makeDebFromRpm" debian7_i386' 
