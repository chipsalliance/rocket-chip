import ammonite.ops._
import mill._
import mill.scalalib._
import mill.scalalib.publish.{License, PomSettings, VersionControl}

import $file.chisel3.build
import $file.firrtl.build


val crossVersions = Seq("2.11.12", "2.12.11")

object rc_chisel3 extends Cross[RocketChipChiselModule](crossVersions: _*)

class RocketChipChiselModule(val crossVersionValue: String) extends chisel3.build.chisel3CrossModule(crossVersionValue) {
  override def millSourcePath = super.millSourcePath / 'chisel3
  override def firrtlModule = Some(firrtl.build.firrtl(crossVersionValue))
}

trait CommonRocketChipModule extends CrossSbtModule with PublishModule {
  override def scalacOptions = Seq("-deprecation","-unchecked","-Xsource:2.11")
  val macroPlugins = Agg(ivy"org.scalamacros:::paradise:2.1.1")
  def scalacPluginIvyDeps = macroPlugins
  def compileIvyDeps = macroPlugins

  // Allow (local) publishing
  def publishVersion = "1.2-SNAPSHOT"
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "edu.berkeley.cs",
    url = "https://github.com/chipsalliance/rocket-chip",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("chipsalliance", "rocket-chip"),
    developers = Seq()
  )
}

// Inherits from CrossScalaModule as opposed to CrossSbtModule to avoid
// the dependency on "src/main/scala" in file paths.
trait CommonRocketChipScalaModule extends CrossScalaModule with PublishModule {
  override def scalacOptions = Seq("-deprecation","-unchecked","-Xsource:2.11")
  val macroPlugins = Agg(ivy"org.scalamacros:::paradise:2.1.1")
  def scalacPluginIvyDeps = macroPlugins
  def compileIvyDeps = macroPlugins

  // Allow (local) publishing
  def publishVersion = "1.2-SNAPSHOT"
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "edu.berkeley.cs",
    url = "https://github.com/chipsalliance/rocket-chip",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("chipsalliance", "rocket-chip"),
    developers = Seq()
  )
}

class MacrosModule(val crossScalaVersion: String) extends CommonRocketChipModule {
}

class HardfloatModule(val crossScalaVersion: String) extends CommonRocketChipModule {
  override def moduleDeps = Seq(rc_chisel3(crossScalaVersion))
}

class ConfigModule(val crossScalaVersion: String) extends CommonRocketChipScalaModule {
  override def millSourcePath = super.millSourcePath / ammonite.ops.up / "api-config-chipsalliance" / 'design / 'craft
}

object rocketchip extends Cross[RocketChipModule](crossVersions: _*)

class RocketChipModule(val crossScalaVersion: String) extends CommonRocketChipModule with PublishModule {
  override def millSourcePath = super.millSourcePath / ammonite.ops.up

  // Internal parts of rocket-chip that are not to be separately published
  object api_config_chipsalliance extends Cross[ConfigModule](crossVersions: _*)
  object macros extends Cross[MacrosModule](crossVersions: _*)
  object hardfloat extends Cross[HardfloatModule](crossVersions: _*)

  override def moduleDeps = Seq(
    rc_chisel3(crossScalaVersion),
    macros(crossScalaVersion),
    api_config_chipsalliance(crossScalaVersion),
    hardfloat(crossScalaVersion),
    firrtl.build.firrtl(crossScalaVersion)
  )

  override def ivyDeps = Agg(ivy"org.json4s::json4s-jackson:3.5.3")

  override def artifactName = "rocketchip"
}
