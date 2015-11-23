name := "nescala"

version := "0.1"

scalaVersion := "2.11.7"

mainClass in (Compile,run) := Some("Console")

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at
    "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Releases" at
    "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2",
                            "com.storm-enroute" %% "macrogl" % "0.4-SNAPSHOT")

javaOptions in run += s"-Djava.library.path=libs/"

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "nescala"
  )

