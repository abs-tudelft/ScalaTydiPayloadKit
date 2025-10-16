ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.17"

val circeVersion = "0.14.14"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies += "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.10"
libraryDependencies +=  "org.scala-lang" % "scala-reflect" % "2.13.17"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

lazy val root = (project in file("."))
  .settings(
    name := "tydi-packaging",
    organization := "nl.tudelft",
  )
