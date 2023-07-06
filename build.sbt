ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

lazy val root = (project in file("."))
  .settings(
    name := "oracle-assignment"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.2.0",
)


val Fs2Version = "3.2.4"
libraryDependencies += "co.fs2" %% "fs2-core" % Fs2Version

libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.7"