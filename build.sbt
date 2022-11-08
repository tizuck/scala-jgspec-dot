ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"
val circeVersion = "0.14.3"

lazy val root = (project in file("."))
  .settings(
    name := "scala-jgfdot",
    idePackagePrefix := Some("com.github.tizuck")
  )

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scala-graph" %% "graph-core" % "1.13.5",
  "org.scala-graph" %% "graph-dot" % "1.13.3"
)
