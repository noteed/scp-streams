#! /usr/bin/env bash

STACK_IMAGE=${1:-7.8.4}

docker run \
  -v `pwd`:/home/gusdev/go \
  -v `pwd`:/home/gusdev/scp-streams \
  images.reesd.com/reesd/stack:$STACK_IMAGE \
  go/gusrun.sh
