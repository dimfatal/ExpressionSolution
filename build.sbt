scalaVersion := "2.13.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0"
addCompilerPlugin(
  "org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full
)

scalacOptions += "-Ymacro-annotations"