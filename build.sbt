ThisBuild / tlBaseVersion := "1.2" // your current series x.y

ThisBuild / organization := "no.arktekk"
ThisBuild / organizationName := "Arktekk"
ThisBuild / startYear := Some(2011)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers ++= List(
  tlGitHubDev("teigen", "Jon Anders Teigen"),
  tlGitHubDev("hamnis", "Erlend Hamnaberg")
)

ThisBuild / sonatypeCredentialHost := xerial.sbt.Sonatype.sonatypeLegacy

val Scala212 = "2.12.20"
val Scala213 = "2.13.16"
val Scala3   = "3.3.4"

ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, Scala3)
ThisBuild / scalaVersion := Scala3 // the default Scala

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .jsSettings(
    tlVersionIntroduced := List("2.12", "2.13", "3").map(_ -> "1.2.0").toMap
  )
  .nativeSettings(
    tlVersionIntroduced := List("2.12", "2.13", "3").map(_ -> "1.2.0").toMap
  )
  .jvmSettings(
    tlVersionIntroduced := Map("2.12" -> "1.1.0", "2.13" -> "1.1.0", "3" -> "1.2.0")
  )
  .in(file("core"))
  .settings(
    name := "uri-template",
    description := "Uri Template",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
      "org.scalatest"          %%% "scalatest"                % "3.2.19" % "test"
    )
  )
