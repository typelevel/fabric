#!/usr/bin/env bash

sbt +clean +test +coreJS/publishSigned +coreJVM/publishSigned +coreNative/publishSigned +parseJS/publishSigned +parseJVM/publishSigned +defineJS/publishSigned +defineJVM/publishSigned sonatypeRelease