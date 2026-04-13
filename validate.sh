#!/bin/bash
set -e

# Simulate CI environment so sbt-typelevel enables fatal warnings
export CI=true
export GITHUB_ACTIONS=true

echo "=== Generating CI workflow ==="
sbt githubWorkflowGenerate

echo "=== Creating license headers ==="
sbt +headerCreate
sbt "+Test / headerCreate"

echo "=== Formatting code ==="
sbt scalafmtSbt
sbt +root/scalafmt
sbt "+Test / scalafmt"

echo "=== Checking headers ==="
sbt +headerCheckAll

echo "=== Checking formatting ==="
sbt "+Test / scalafmtCheck"
sbt scalafmtSbtCheck

echo "=== Compiling (with fatal warnings) ==="
sbt +clean +compile

echo "=== Running tests ==="
sbt +test

echo "=== Validation complete ==="
