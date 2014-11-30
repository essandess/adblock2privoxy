#!/bin/bash

parallel ::: \
'./run_aws_build.sh ami-1eb35469 fedora make_rpm "distribution/rpmbuild/RPMS" fedora20_64' \
'./run_aws_build.sh ami-0bac577c fedora make_rpm "distribution/rpmbuild/RPMS" fedora20_i386' 