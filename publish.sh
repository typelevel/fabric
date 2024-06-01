#!/usr/bin/env bash

set -e

./validate.sh
sbt +clean
sbt +compile
#sbt +test
sbt docs/mdoc
sbt "util/runMain util.DoRelease $@"