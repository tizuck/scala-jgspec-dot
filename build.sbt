ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"
val circeVersion = "0.14.3"

val commonSettings = Seq(
  organizationName := "Tilman Zuckmantel",
  startYear := Some(2022),
  headerLicense := Some(HeaderLicense.ALv2("2022","Tilman Zuckmantel"))
)

lazy val root = Project(id = "tizu-jgspec-dot", base = file("."))
  .settings(commonSettings : _*)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.scala-graph" %% "graph-core" % "1.13.5",
      "org.scala-graph" %% "graph-dot" % "1.13.3",
      "org.scalatest" %% "scalatest" % "3.2.14" % Test,
    ),
    name := "scala-jgfdot",
    idePackagePrefix := Some("com.github.tizuck")

  )
