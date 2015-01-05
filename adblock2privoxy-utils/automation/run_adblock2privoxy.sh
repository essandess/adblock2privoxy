#!/bin/bash
set -e

AMI=ami-1eb35469
USER=fedora
COMMAND=$3
BUILD_ID=$(date -u "+%Y-%m-%d_%H:%M:%S")

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
sftp -i ~/.ssh/ab2p.pem -o "IdentitiesOnly yes" -o "StrictHostKeyChecking no" $USER@$INSTANCE_IP<<END
put *.rpm
exit
END

echo "Install and run adblock2privoxy"
ssh -t -t -i ~/.ssh/ab2p.pem -o "IdentitiesOnly yes" -o "StrictHostKeyChecking no" $USER@$INSTANCE_IP<<END
rpm -i *.rpm
yum install p7zip

mkdir -p out/privoxy
adblock2privoxy -p out/privoxy -w out/web -d www.example.com https://easylist-downloads.adblockplus.org/easylist.txt https://easylist-downloads.adblockplus.org/easyprivacy.txt https://easylist-downloads.adblockplus.org/advblock.txt https://easylist-downloads.adblockplus.org/bitblock.txt https://easylist-downloads.adblockplus.org/cntblock.txt
7za a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on all.7z out/
rm -rf out

mkdir -p out/privoxy
adblock2privoxy -p out/privoxy -w out/web -d www.example.com https://easylist-downloads.adblockplus.org/easylist.txt
7za a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on easylist.7z out/
rm -rf out

./$COMMAND.sh | tee build.log
exit
END

echo "Download result from $RESULT_PATH"
sftp -i ~/.ssh/ab2p.pem -o "IdentitiesOnly yes" -o "StrictHostKeyChecking no" $USER@$INSTANCE_IP<<END
get *.7z $BUILD_ID
exit
END

echo "Upload result to s3"
aws s3 cp $BUILD_ID\*.7z s3:\\ab2p.zubr.me\

echo "Terminate instance"
aws ec2 terminate-instances --instance-ids "$INSTANCE_ID"
