val scala3Version = "3.1.3"
// val scala3Version = "2.13.8"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Scala 3 Project Template",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.typesafe.play" %% "play-json" % "2.10.0-RC6"
    )
  )
