name := "nescala"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2",
                            "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
                            "com.typesafe" % "config" % "1.3.0",
                            "org.lwjgl.lwjgl" % "lwjgl" % "2.9.1",
                            "com.nativelibs4java" %% "scalaxy-streams" % "0.3.4" % "provided",
                            "net.java.jinput" % "jinput-platform" % "2.0.6" pomOnly()
)

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    autoCompilerPlugins := true,
    addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4"),
    mainClass in Compile := Some("com.owlandrews.nescala.ui.Run"),
    fork in run := true,
    scalacOptions ++= Seq("-Xplugin-require:scalaxy-streams", "-feature", "-optimise", "-Yclosure-elim", "-Yinline"),
    javaOptions in run ++= Seq("-XX:UseSSE=3", "-XX:+UseConcMarkSweepGC", "-Xms256m"),
    javaOptions += "-Djava.library.path=libs/",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.owlandrews.nescala"
  )