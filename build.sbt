// See LICENSE.Berkeley for license details.

import sbt.complete._
import sbt.complete.DefaultParsers._
import xerial.sbt.pack._
import sys.process._

enablePlugins(PackPlugin)

lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version      := "1.2",
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.12.4", "2.11.12"),
  parallelExecution in Global := false,
  traceLevel   := 15,
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "3.5.3"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

lazy val chisel = (project in file("chisel3")).settings(commonSettings)
lazy val hardfloat  = project.dependsOn(chisel).settings(commonSettings)
  .settings(crossScalaVersions := Seq("2.11.12", "2.12.4"))
lazy val macros = (project in file("macros")).settings(commonSettings)
lazy val rocketchip = (project in file("."))
  .settings(commonSettings, chipSettings)
  .dependsOn(chisel, hardfloat, macros)
  .aggregate(chisel, hardfloat, macros) // <-- means the running task on rocketchip is also run by aggregate tasks

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

