// See LICENSE.Berkeley for license details.

import sbt._
import Keys._
import complete._
import complete.DefaultParsers._
import xerial.sbt.Pack._

object BuildSettings extends Build {

  // The Chisel projects we know we'll require.
  // This could be any (or all) of the BIG4 projects
  val chiselDeps = chisel.dependencies(Seq(
    ("edu.berkeley.cs" %% "firrtl" % "1.1-SNAPSHOT", "firrtl"),
    ("edu.berkeley.cs" %% "chisel3" % "3.1-SNAPSHOT", "chisel3")
))

  override lazy val settings = super.settings ++ Seq(
    organization := "berkeley",
    version      := "1.2",
    scalaVersion := "2.11.7",
    parallelExecution in Global := false,
    traceLevel   := 15,
    scalacOptions ++= Seq("-deprecation","-unchecked"),
    libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )

//  lazy val chisel3 = project in file("chisel3")
  lazy val hardfloat  = project.dependsOn(chiselDeps.projects.map(classpathDependency(_)): _*)
  lazy val rocketchip = (project in file(".")).settings(chipSettings).dependsOn(chiselDeps.projects.map(classpathDependency(_)): _*, hardfloat)

  lazy val addons = settingKey[Seq[String]]("list of addons used for this build")
  lazy val make = inputKey[Unit]("trigger backend-specific makefile command")
  val setMake = NotSpace ~ ( Space ~> NotSpace )

  val chipSettings = packAutoSettings ++ Seq(
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
      s"make -C $makeDir  -j $jobs $target" !
    }
  )
}
