#!/bin/bash

parallel ::: \
'./run_aws_build.sh ami-1eb35469 fedora makeFedoraRpm fedora20_64' \
'./run_aws_build.sh ami-0bac577c fedora makeFedoraRpm fedora20_i386' 
