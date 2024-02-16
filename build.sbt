ThisBuild / version := "0.1.0-SNAPSHOT"

// Scala 2.x version https://www.scala-lang.org/download/all.html
ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "HealthCareDay"
  )

// https://www.scalatest.org/install
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"

// https://www.scalatest.org/plus/scalacheck
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % "test"

// https://scalacheck.org/download.html, do not trust the version shown there but check from "The Central Repository":
// https://central.sonatype.com/artifact/org.scalacheck/scalacheck_2.13
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"

// https://github.com/clintval/bimap/tree/main
libraryDependencies += "io.cvbio.collection.mutable" %% "bimap" % "2.0.0"

ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")
