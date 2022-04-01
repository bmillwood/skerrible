#!/usr/bin/env bash
set -ux
while sleep 1
do
  inotifywait --quiet -e modify -e delete elm.json src &
  elm make --output=elm.js src/Main.elm
  if [[ "$?" == 0 && "$#" == 2 && "$1" == "--upload" ]]
  then
      scp elm.js "$2"
  fi
  wait %inotifywait
done
