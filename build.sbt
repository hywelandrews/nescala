name := "nescala"

version := "0.1"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at
    "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Releases" at
    "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2",
                            "com.typesafe" % "config" % "1.3.0",
                            "com.storm-enroute" %% "macrogl" % "0.4-SNAPSHOT")

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    mainClass in (Compile,run) := Some("ui.Run"),
    fork in run := true,
    javaOptions in run += "-XX:UseSSE=3",
    javaOptions in run += "-XX:+UseConcMarkSweepGC",
    javaOptions in run +=  "-Xms128m",
    javaOptions += "-Djava.library.path=libs/",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "nescala"
  )