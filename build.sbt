name := "nescala"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2",
                            "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
                            "com.typesafe" % "config" % "1.3.0",
                            "org.lwjgl.lwjgl" % "lwjgl" % "2.9.1")

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    mainClass in (Compile,run) := Some("ui.Run"),
    fork in run := true,
    scalacOptions in Compile += "-feature",
    javaOptions in run += "-XX:UseSSE=3",
    javaOptions in run += "-XX:+UseConcMarkSweepGC",
    javaOptions in run +=  "-Xms128m",
    javaOptions += "-Djava.library.path=libs/",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "nescala"
  )