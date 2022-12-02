#!/usr/bin/env bash

set -e

sbt githubWorkflowGenerate
sbt scalafmt
sbt "Test / scalafmt"
sbt +clean
sbt +compile
sbt +test
sbt docs/mdoc
sbt "util/runMain util.UpdateReadme"