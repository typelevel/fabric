#!/usr/bin/env bash

set -e

sbt +clean +compile +test
sbt docs/mdoc
sbt +publishSigned
sbt sonatypeBundleRelease