name := "nescala"

version := "1.0"

scalaVersion := "2.11.7"

mainClass in (Compile,run) := Some("Console")

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "nescala"
  )