#!/bin/bash
echo "get latest binary"
rm run/*.rpm
latestRpm=$(aws s3 ls s3://ab2p|sed -r -e 's/^.*(adblock2privoxy.*fc20\.x86_64\.rpm)/\1/;t;d'|sort|tail -n 1)
aws s3 cp s3://ab2p/$latestRpm run/

./run_aws_build.sh ami-1eb35469 fedora "run" "ab2p" ab2p_output
