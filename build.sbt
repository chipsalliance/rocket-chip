// See LICENSE.Berkeley for license details.

import sbt.complete.DefaultParsers._
import scala.sys.process._

enablePlugins(PackPlugin)

lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version      := "1.2-SNAPSHOT",
  scalaVersion := "2.12.10",
  parallelExecution in Global := false,
  traceLevel   := 15,
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "3.6.1"),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.8" % "test"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal
  ),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { x => false },
  pomExtra := <url>https://github.com/chipsalliance/rocket-chip</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
    </license>
    <license>
      <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/chipsalliance/rocketchip.git</url>
      <connection>scm:git:github.com/chipsalliance/rocketchip.git</connection>
    </scm>,
  publishTo := {
    val v = version.value
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) {
      Some("snapshots" at nexus + "content/repositories/snapshots")
    }
    else {
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  }
)

lazy val chisel = (project in file("chisel3")).settings(commonSettings)

def dependOnChisel(prj: Project) = {
  if (sys.props.contains("ROCKET_USE_MAVEN")) {
    prj.settings(
      libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % "3.2-SNAPSHOT")
    )
  } else {
    prj.dependsOn(chisel)
  }
}

lazy val `api-config-chipsalliance` = (project in file("api-config-chipsalliance/build-rules/sbt"))
  .settings(commonSettings)
  .settings(publishArtifact := false)
lazy val hardfloat  = dependOnChisel(project).settings(commonSettings)
  .settings(publishArtifact := false)
lazy val `rocket-macros` = (project in file("macros")).settings(commonSettings)
  .settings(publishArtifact := false)
lazy val rocketchip = dependOnChisel(project in file("."))
  .settings(commonSettings, chipSettings)
  .dependsOn(`api-config-chipsalliance` % "compile-internal;test-internal")
  .dependsOn(hardfloat % "compile-internal;test-internal")
  .dependsOn(`rocket-macros` % "compile-internal;test-internal")
  .settings(
      aggregate := false,
      // Include macro classes, resources, and sources in main jar.
      mappings in (Compile, packageBin) ++= (mappings in (`api-config-chipsalliance`, Compile, packageBin)).value,
      mappings in (Compile, packageSrc) ++= (mappings in (`api-config-chipsalliance`, Compile, packageSrc)).value,
      mappings in (Compile, packageBin) ++= (mappings in (hardfloat, Compile, packageBin)).value,
      mappings in (Compile, packageSrc) ++= (mappings in (hardfloat, Compile, packageSrc)).value,
      mappings in (Compile, packageBin) ++= (mappings in (`rocket-macros`, Compile, packageBin)).value,
      mappings in (Compile, packageSrc) ++= (mappings in (`rocket-macros`, Compile, packageSrc)).value,
      exportJars := true,
      Test / unmanagedBase := baseDirectory.value / "test_lib"
  )


lazy val addons = settingKey[Seq[String]]("list of addons used for this build")
lazy val make = inputKey[Unit]("trigger backend-specific makefile command")
val setMake = NotSpace ~ ( Space ~> NotSpace )

lazy val chipSettings = Seq(
  addons := {
    val a = sys.env.getOrElse("ROCKETCHIP_ADDONS", "")
    println(s"Using addons: $a")
    a.split(" ")
  },
  unmanagedSourceDirectories in Compile ++= addons.value.map(baseDirectory.value / _ / "src/main/scala"),
  mainClass in (Compile, run) := Some("rocketchip.Generator"),
  make := {
    val jobs = java.lang.Runtime.getRuntime.availableProcessors
    val (makeDir, target) = setMake.parsed
    (run in Compile).evaluated
    s"make -C $makeDir  -j $jobs $target".!
  }
)

