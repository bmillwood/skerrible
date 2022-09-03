#!/usr/bin/env bash
set -ux

killGrandchild() {
  pkill --parent $(pgrep --parent $1)
}

while sleep 1
do
  inotifywait --quiet -e modify -e delete protocol server/{src,tests} skerrible.cabal &
  if cabal build "$@"
  then
    cabal run skerrible-server -- static-root &
    cabal test
    wait %inotifywait
    jobs -x killGrandchild %?run
    wait %?run
  else
    wait %inotifywait
  fi
done
