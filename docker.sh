#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
if [ -n "$(git status --porcelain)" ]
then
    echo "Repository unclean, will not build" >&2
    exit 1
fi
tag=$(git rev-parse --short HEAD)
docker build --build-arg version=$tag --tag skerrible:"$tag" .
