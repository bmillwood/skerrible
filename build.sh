#!/usr/bin/env bash
set -ux

function waitForSource() {
  inotifywait --quiet -e modify -e delete protocol server/src server/tests
}

killGrandchild() {
  pkill --parent $(pgrep --parent $1)
}

while sleep 1
do
  if cabal build
  then
    cabal run skerrible-server -- static-root &
    cabal test &
    waitForSource
    jobs -x killGrandchild %%
    wait
  else
    waitForSource
  fi
done
