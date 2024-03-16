#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
dockername=skerrible-build
if [ -z "$(docker container ls --all --quiet --filter name="^$dockername"'$')" ]
then
    mkdir -p ubuntu
    docker run --name "$dockername" --mount type=bind,src=$PWD,dst=/mnt haskell:9.4 bash -c 'cd /mnt; cabal update; cabal install --only-dependencies'
fi
if [ -z "$(docker container ls --quiet --filter name="^$dockername"'$')" ]
then
    docker start "$dockername"
fi
docker exec "$dockername" bash -c 'cd /mnt; cabal install --install-method=copy --installdir=/mnt/ubuntu'
