import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += ok,
    libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.16.0",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    libraryDependencies += "com.typesafe.play" % "play-json_2.12" % "2.6.0",
    libraryDependencies += "com.github.melrief" %% "purecsv" % "0.1.0"

  )
