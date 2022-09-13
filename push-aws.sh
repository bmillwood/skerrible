#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
tag=$1
aws_account_id=$(aws sts get-caller-identity | jq .Account --raw-output)
aws_region=$(aws configure get region)
aws_registry="$aws_account_id.dkr.ecr.$aws_region.amazonaws.com"
repo_name=skerrible
local_image_name=$repo_name
aws ecr get-login-password --region "$aws_region" \
    | docker login --username AWS --password-stdin "$aws_registry"
docker tag "$local_image_name:$tag" "$aws_registry/$repo_name:$tag"
docker push "$aws_registry/$repo_name:$tag"
