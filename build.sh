#! /usr/bin/env bash

STACK_IMAGE=${1:-7.10.3}

docker run \
  -v $(pwd)/../sha-streams:/home/gusdev/sha-streams \
  -v $(pwd)/../scp-streams:/home/gusdev/scp-streams \
  images.reesd.com/reesd/stack:$STACK_IMAGE \
  scp-streams/gusrun.sh
