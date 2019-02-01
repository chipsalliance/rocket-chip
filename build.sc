import ammonite.ops._
import mill._
import mill.scalalib._

import $file.chisel3.build
import $file.firrtl.build


val crossVersions = Seq("2.11.12", "2.12.11")

object rc_chisel3 extends Cross[RocketChipChiselModule](crossVersions: _*)

class RocketChipChiselModule(val crossVersionValue: String) extends chisel3.build.chisel3CrossModule(crossVersionValue) {
  override def millSourcePath = super.millSourcePath / 'chisel3
  override def firrtlModule = Some(firrtl.build.firrtl(crossVersionValue))
}

trait CommonRocketChipModule extends CrossSbtModule {
  override def scalacOptions = Seq("-deprecation","-unchecked","-Xsource:2.11")
  val macroPlugins = Agg(ivy"org.scalamacros:::paradise:2.1.1")
  def scalacPluginIvyDeps = macroPlugins
  def compileIvyDeps = macroPlugins
}

// Inherits from CrossScalaModule as opposed to CrossSbtModule to avoid
// the dependency on "src/main/scala" in file paths.
trait CommonRocketChipScalaModule extends CrossScalaModule {
  override def scalacOptions = Seq("-deprecation","-unchecked","-Xsource:2.11")
  val macroPlugins = Agg(ivy"org.scalamacros:::paradise:2.1.1")
  def scalacPluginIvyDeps = macroPlugins
  def compileIvyDeps = macroPlugins
}

object macros extends Cross[MacrosModule](crossVersions: _*)

class MacrosModule(val crossScalaVersion: String) extends CommonRocketChipModule {
}

object hardfloat extends Cross[HardfloatModule](crossVersions: _*)

class HardfloatModule(val crossScalaVersion: String) extends CommonRocketChipModule {
  override def moduleDeps = Seq(rc_chisel3(crossScalaVersion))
}

class ConfigModule(val crossScalaVersion: String) extends CommonRocketChipScalaModule {
  override def millSourcePath = super.millSourcePath / ammonite.ops.up / "api-config-chipsalliance" / 'design / 'craft
}

object api_config_chipsalliance extends Cross[ConfigModule](crossVersions: _*)

object rocketchip extends Cross[RocketChipModule](crossVersions: _*) {
  def defaultVersion: String = crossVersions.head

  def compile = T{
    rocketchip(defaultVersion).compile()
  }

  def jar = T{
    rocketchip(defaultVersion).jar()
  }

  //def test = T{
  //  rocketchip(defaultVersion).test.test()
  //}

  //def publishLocal = T{
  //  rocketchip(defaultVersion).publishLocal()
  //}

  //def docJar = T{
  //  rocketchip(defaultVersion).docJar()
  //}
}

class RocketChipModule(val crossScalaVersion: String) extends CommonRocketChipModule {

  override def millSourcePath = super.millSourcePath / ammonite.ops.up

  override def moduleDeps = Seq(
    rc_chisel3(crossScalaVersion),
    macros(crossScalaVersion),
    api_config_chipsalliance(crossScalaVersion),
    hardfloat(crossScalaVersion),
    firrtl.build.firrtl(crossScalaVersion)
  )

  override def ivyDeps = Agg(ivy"org.json4s::json4s-jackson:3.5.3")
}
