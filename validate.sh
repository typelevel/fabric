sbt githubWorkflowGenerate
sbt +headerCreate
sbt "+Test / headerCreate"
sbt +scalafmt
sbt "+Test / scalafmt"