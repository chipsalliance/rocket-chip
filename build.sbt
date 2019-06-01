// See LICENSE.Berkeley for license details.

import sbt.complete._
import sbt.complete.DefaultParsers._
//import xerial.sbt.pack._
import sys.process._

//enablePlugins(PackPlugin)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "firrtl" -> "1.2-SNAPSHOT",
  "chisel3" -> "3.2-SNAPSHOT"
  )

lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version      := "1.2",
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.12.8", "2.11.12"),
  parallelExecution in Global := false,
  traceLevel   := 15,
  maxErrors    := 5,
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "3.6.1"),
  libraryDependencies ++= Seq("chisel3").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) },
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
)

lazy val macros = (project in file("macros")).settings(commonSettings)
lazy val fusion = (project in file("."))
  .settings(commonSettings, chipSettings)
  .dependsOn(macros)
  .aggregate(macros) // <-- means the running task on rocketchip is also run by aggregate tasks

lazy val addons = settingKey[Seq[String]]("list of addons used for this build")
lazy val make = inputKey[Unit]("trigger backend-specific makefile command")
val setMake = NotSpace ~ ( Space ~> NotSpace )

val chipSettings = Seq(
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


// Lint
//scapegoatVersion in ThisBuild := "1.3.8"
//scalaBinaryVersion in ThisBuild := "2.12"
