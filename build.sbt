// See LICENSE.Berkeley for license details.

import sbt.complete.DefaultParsers._
import scala.sys.process._

enablePlugins(PackPlugin)

// This needs to stay in sync with the chisel3 and firrtl git submodules
val chiselVersion = "3.4.0"

lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version      := "1.2-SNAPSHOT",
  scalaVersion := "2.12.10",
  parallelExecution in Global := false,
  traceLevel   := 15,
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "3.6.1"),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.0" % "test"),
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

lazy val chiselRef = ProjectRef(workspaceDirectory / "chisel3", "chisel")
lazy val chiselLib = "edu.berkeley.cs" %% "chisel3" % chiselVersion
// While not built from source, *must* be in sync with the chisel3 git submodule
// Building from source requires extending sbt-sriracha or a similar plugin and
//   keeping scalaVersion in sync with chisel3 to the minor version
lazy val chiselPluginLib = "edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full

lazy val `api-config-chipsalliance` = (project in file("api-config-chipsalliance/build-rules/sbt"))
  .settings(commonSettings)
  .settings(publishArtifact := false)
lazy val hardfloat  = (project in file("hardfloat"))
  .sourceDependency(chiselRef, chiselLib)
  .settings(addCompilerPlugin(chiselPluginLib))
  .settings(commonSettings)
  .settings(publishArtifact := false)
lazy val `rocket-macros` = (project in file("macros")).settings(commonSettings)
  .settings(publishArtifact := false)
lazy val rocketchip = (project in file("."))
  .sourceDependency(chiselRef, chiselLib)
  .settings(addCompilerPlugin(chiselPluginLib))
  .settings(commonSettings, chipSettings)
  .dependsOn(`api-config-chipsalliance`)
  .dependsOn(hardfloat)
  .dependsOn(`rocket-macros`)
  .settings( // Assembly settings
    assembly / test := {},
    assembly / assemblyJarName := "rocketchip.jar",
    assembly / assemblyOutputPath := baseDirectory.value / "rocketchip.jar"
  )
  .settings( // Settings for scalafix
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions += "-Ywarn-unused-import"
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

// mdoc documentation target
lazy val docs = project
  .in(file("docs-target"))
  .dependsOn(rocketchip)
  .enablePlugins(MdocPlugin)
  .settings(commonSettings)
  .settings(
      scalacOptions += "-language:reflectiveCalls",
      mdocIn := file("docs/src"),
      mdocOut := file("docs/generated"),
      mdocExtraArguments := Seq("--cwd", "docs"),
      mdocVariables := Map(
        // build dir for mdoc programs to dump temp files
        "BUILD_DIR" -> "docs-target"
      )
  )
