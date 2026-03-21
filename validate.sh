sbt githubWorkflowGenerate
sbt +headerCreate
sbt "+Test / headerCreate"
sbt scalafmtSbt
sbt +root/scalafmt
sbt "+Test / scalafmt"
sbt "+Test / scalafmtCheck"