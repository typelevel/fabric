#!/usr/bin/env bash

set -e

export CI=true

./validate.sh
sbt +root/doc
sbt docs/mdoc
sbt "util/runMain util.DoRelease $@"