ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"
val circeVersion = "0.14.3"

organizationName := "Tilman Zuckmantel"
startYear := Some(2022)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

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
  "org.scala-graph" %% "graph-dot" % "1.13.3",
  "org.scalatest" %% "scalatest" % "3.2.14" % Test,
)
