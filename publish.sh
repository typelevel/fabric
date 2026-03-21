#!/usr/bin/env bash

set -e

export CI=true

./validate.sh
sbt +clean
sbt +root/compile
sbt +root/test
sbt docs/mdoc
sbt "util/runMain util.DoRelease $@"