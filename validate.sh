sbt githubWorkflowGenerate
sbt +headerCreate
sbt "+Test / headerCreate"
sbt scalafmtSbt
sbt +scalafmt
sbt "+Test / scalafmt"