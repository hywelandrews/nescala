name := "nescala"

scalaVersion := "2.13.8"

libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-swing" % "2.1.1",
                            "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
                            "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0",
                            "com.typesafe" % "config" % "1.3.3",
                            "org.lwjgl.lwjgl" % "lwjgl" % "2.9.3",
                            "net.java.jinput" % "jinput-platform" % "2.0.7" pomOnly()
)

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    autoCompilerPlugins := true,
    mainClass in Compile := Some("com.owlandrews.nescala.ui.Run"),
    fork := true,
    scalacOptions ++= Seq("-feature", "-deprecation"),
    javaOptions ++= Seq("-XX:UseSSE=3", "-Xms256m", "-XX:+UseParNewGC", "-XX:+UseConcMarkSweepGC", "-Xloggc:gc.log", "-XX:+PrintGCDetails", "-XX:+PrintGCApplicationStoppedTime", "-XX:+PrintGCApplicationConcurrentTime", "-XX:MetaspaceSize=32m"),
    javaOptions in (Compile, run) += "-Djava.library.path=libs/",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.owlandrews.nescala"
  )