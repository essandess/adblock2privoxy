#!/bin/bash
set -e

AMI=$1
USER=$2
UPLOAD=$3
COMMAND=$4
BUILD_ID=$5

echo "build started, see $BUILD_ID/build.log for result"
mkdir -p $BUILD_ID
exec > $BUILD_ID/build.log 2>&1

echo "create instance of $AMI"

INSTANCE_ID=$(aws ec2 run-instances \
--image-id "$AMI" \
--count 1 \
--key-name ab2p \
--security-groups launch-wizard-2 \
--instance-type c1.medium \
--block-device-mappings '[{"DeviceName": "/dev/sda1","Ebs": {"VolumeSize": 10,"DeleteOnTermination": true,"VolumeType": "standard"}}]' \
| sed -n -r '/InstanceId/ {s/.*:\s"([[:alnum:]-]+)".*/\1/;p}')

echo "$INSTANCE_ID created"

echo "Set trap to terminate instance on exit"
trap "aws ec2 terminate-instances --instance-ids $INSTANCE_ID" EXIT

echo "Waiting for instance ready"
INSTANCE_STATUS=""
until [ "$INSTANCE_STATUS" == "running" ]; do
    echo -n "."
    sleep 1
    INSTANCE_STATUS=$(aws ec2 describe-instances --instance-ids "$INSTANCE_ID" | sed -r 's@.*"Name": "([[:alpha:]]+)".*@\1@;t;d')
done
echo ""


INSTANCE_IP=$(aws ec2 describe-instances --instance-ids "$INSTANCE_ID" | sed -r 's@.*"PublicIpAddress": "([0-9.]+)".*@\1@;t;d')
echo "Instance IP address: $INSTANCE_IP"

echo "Waiting for ssh ready for user $USER"
INSTANCE_STATUS=""
until ssh -q -i ~/.ssh/ab2p.pem -o "IdentitiesOnly yes" -o "StrictHostKeyChecking no" -o "NumberOfPasswordPrompts 0" $USER@$INSTANCE_IP exit; do
    echo -n "."
    sleep 1
done

echo "Upload files"
dirName=$(basename "$UPLOAD")
sftp -i ~/.ssh/ab2p.pem -o "IdentitiesOnly yes" -o "StrictHostKeyChecking no" $USER@$INSTANCE_IP<<END
mkdir $dirName
put -r $UPLOAD
exit
END

echo "Run build with $COMMAND"
ssh -t -t -i ~/.ssh/ab2p.pem -o "IdentitiesOnly yes" -o "StrictHostKeyChecking no" $USER@$INSTANCE_IP "$dirName/$COMMAND.sh | tee build.log"

echo "Download result from /result"
sftp -i ~/.ssh/ab2p.pem -o "IdentitiesOnly yes" -o "StrictHostKeyChecking no" $USER@$INSTANCE_IP<<END
get -r result/adblock2privoxy* $BUILD_ID
exit
END

echo "Upload result to s3"
aws s3 cp $BUILD_ID/adblock2privoxy* s3://ab2p/

