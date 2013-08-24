import xerial.sbt.Pack._

name := "kirakira-annatan"

organization := "icfpc2013.kirakira"

version := "0.1"

scalaVersion := "2.10.2"

scalacOptions += "-deprecation"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "spray" at "http://repo.spray.io/"

libraryDependencies ++= Seq(
  "org.scalaj" %% "scalaj-http" % "0.3.9" exclude("junit", "junit"),
  "io.spray" %%  "spray-json" % "1.2.5",
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "org.scalatest" %% "scalatest" % "2.0.RC1-SNAP2" % "test"
)

mainClass in (Compile, run) := Some("icfpc2013.kirakira.TrainingSolver")

packSettings

packMain := Map("training" -> "icfpc2013.kirakira.TrainingSolver",
                "contest" -> "icfpc2013.kirakira.ContestSolver")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource
