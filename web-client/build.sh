#!/usr/bin/env bash
set -ux
while sleep 1
do
  elm make --output=elm.js src/Main.elm
  if [[ "$?" == 0 && "$#" == 2 && "$1" == "--upload" ]]
  then
      scp elm.js $2
  fi
  inotifywait --quiet -e modify -e delete . src src/*
done
