import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "berkeley"
  val buildVersion = "1.1"
  val buildScalaVersion = "2.9.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    //unmanagedBase <<= baseDirectory { base => base / ".." / custom_lib" },
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  )
}

object ChiselBuild extends Build{
  import BuildSettings._

  lazy val chisel = Project("chisel", file("chisel"), settings = buildSettings)
  lazy val hardfloat = Project("hardfloat", file("hardfloat"), settings = buildSettings) dependsOn(chisel)
  lazy val hwacha = Project("hwacha", file("hwacha"), settings = buildSettings) dependsOn(hardfloat,chisel)
  lazy val uncore = Project("uncore", file("uncore"), settings = buildSettings) dependsOn(chisel)
  lazy val rocket = Project("rocket", file("rocket"), settings = buildSettings) dependsOn(uncore,hwacha,hardfloat,chisel)
}
