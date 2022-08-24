ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "scala-circuit",
    libraryDependencies  ++= Seq(
      "org.scalanlp" %% "breeze" % "2.0.1-RC1",
      "org.scalanlp" %% "breeze-viz" % "2.0.1-RC1",
      // TODO https://github.com/scalanlp/breeze/wiki/Installation#enabling-native-code
      //"org.scalanlp" %% "breeze-natives" % "2.0.1-RC1"
      "org.scalatest" %% "scalatest" % "3.2.13" % "test"
    )
  )
