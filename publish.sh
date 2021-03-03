#!/usr/bin/env bash

sbt +clean +test +coreJS/publishSigned +coreJVM/publishSigned +coreNative/publishSigned +parseJS/publishSigned +parseJVM/publishSigned sonatypeRelease