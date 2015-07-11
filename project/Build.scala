import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt.Keys._
import sbt._

import scala.xml.Group

object Build extends sbt.Build {
  val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization         := "no.arktekk",
    scalaVersion         := "2.11.7",
    publishTo           <<= isSnapshot {
      case true  ⇒ Some("Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
      case false ⇒ Some("Sonatype Nexus Staging"   at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    credentials          += Credentials(Path.userHome / ".sbt" / "arktekk-credentials")
  )

  val manifestSetting = packageOptions <+= (name, version, organization) map {
    (title, version, vendor) =>
      Package.ManifestAttributes(
        "Created-By" -> "Simple Build Tool",
        "Built-By" -> System.getProperty("user.name"),
        "Build-Jdk" -> System.getProperty("java.version"),
        "Specification-Title" -> title,
        "Specification-Version" -> version,
        "Specification-Vendor" -> vendor,
        "Implementation-Title" -> title,
        "Implementation-Version" -> version,
        "Implementation-Vendor-Id" -> vendor,
        "Implementation-Vendor" -> vendor
      )
  }

  // Things we care about primarily because Maven Central demands them
  val mavenCentralFrouFrou = Seq(
    homepage := Some(new URL("http://github.com/arktekk/uri-template")),
    startYear := Some(2011),
    licenses := Seq(("Apache 2", new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))),
    pomExtra <<= (pomExtra, name, description) {(pom, name, desc) => pom ++ Group(
      <scm>
        <url>https://github.com/arktekk/uri-template</url>
        <connection>scm:git:git://github.com/arktekk/uri-template.git</connection>
        <developerConnection>scm:git:git@github.com:arktekk/uri-template.git</developerConnection>
      </scm>
      <developers>
        <developer>
          <id>teigen</id>
          <name>Jon-Anders Teigen</name>
          <url>http://twitter.com/jteigen</url>
        </developer>
      </developers>
    )}
  )

  val sourceMapTransform = (isSnapshot, version) map {
    case (true, v) ⇒
      val a = new java.io.File("").toURI.toString.replaceFirst("/$", "")
      val g = s"https://raw.githubusercontent.com/arktekk/uri-template/$v"
      s"-P:scalajs:mapSourceURI:$a->$g/"
    case (false, _) ⇒ ""
  }

  val uriTemplate = crossProject.in(file("."))
    .settings(buildSettings ++ mavenCentralFrouFrou :_*)
    .settings(
      name                 := "uri-template",
      description          := "URI Template",
      libraryDependencies  += "org.scalatest" %%% "scalatest" % "3.0.0-M6" % "test",
      manifestSetting
    ).jsSettings(
      scalacOptions       <+= sourceMapTransform,
      libraryDependencies  += "org.scala-js" %%% "scala-parser-combinators" % "1.0.2"
    ).jvmSettings(
      libraryDependencies  += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
    )

  val jvm = uriTemplate.jvm
  val js  = uriTemplate.js

  val root = project.aggregate(jvm, js).settings(publish := {}, publishLocal := {})
}
