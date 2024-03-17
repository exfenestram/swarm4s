import Dependencies._

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.simularity"
ThisBuild / organizationName := "swarm4s"

lazy val root = (project in file("."))
  .settings(
    name := "swarm4s",
    libraryDependencies += munit % Test,
    // Get parallel collections
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
