ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode",
    libraryDependencies ++= Seq(
      "com.github.pathikrit"   %% "better-files"               % "3.9.2",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    )
  )
