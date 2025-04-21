ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "slox"
  )
