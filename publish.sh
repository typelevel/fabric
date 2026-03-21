#!/usr/bin/env bash

set -e

./validate.sh
sbt +clean
sbt +root/compile
sbt +test
sbt docs/mdoc
sbt "util/runMain util.DoRelease $@"