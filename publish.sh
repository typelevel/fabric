#!/usr/bin/env bash

set -e

sbt +clean +compile
sbt +test
sbt docs/mdoc
sbt +publishSigned
sbt sonatypeBundleRelease