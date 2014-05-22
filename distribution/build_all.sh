#!/bin/bash

parallel ::: \
'./run_aws_build.sh ami-1eb35469 fedora make_rpm "distribution/rpmbuild/RPMS" fedora20_64' \
'./run_aws_build.sh ami-0bac577c fedora make_rpm "distribution/rpmbuild/RPMS" fedora20_i386' \
'./run_aws_build.sh ami-29a2595e fedora make_rpm "distribution/rpmbuild/RPMS" fedora19_64' \
'./run_aws_build.sh ami-9f031eeb fedora make_rpm "distribution/rpmbuild/RPMS" fedora19_i386' 
#'./run_aws_build.sh ami-9368ade4 ec2-user make_rpm "distribution/rpmbuild/RPMS" redhat65_x64' \
#'./run_aws_build.sh ami-916ca9e6 ec2-user make_rpm "distribution/rpmbuild/RPMS" redhat65_i386' \
#'./run_aws_build.sh ami-8d1109f9 fedora make_rpm "distribution/rpmbuild/RPMS" opensuse11_x64' \
#'./run_aws_build.sh ami-fd110989 fedora make_rpm "distribution/rpmbuild/RPMS" opensuse11_i386' 