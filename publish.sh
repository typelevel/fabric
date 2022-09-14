#!/usr/bin/env bash

sbt +clean +test +coreJS/publishSigned +coreJVM/publishSigned +coreNative/publishSigned +ioJS/publishSigned +ioJVM/publishSigned +defineJS/publishSigned +defineJVM/publishSigned sonatypeRelease