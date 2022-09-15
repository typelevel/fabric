#!/usr/bin/env bash

set -e

sbt +clean +compile
sbt +test
sbt docs/mdoc
sbt +coreJS/publishSigned +coreJVM/publishSigned +coreNative/publishSigned +ioJS/publishSigned +ioJVM/publishSigned +defineJS/publishSigned +defineJVM/publishSigned
sbt sonatypeRelease