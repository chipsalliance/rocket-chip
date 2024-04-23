import mill._
import mill.scalalib._

import $file.common

val scala = "2.13.12"
val chisel = "6.3.0"

val json4sJackson = ivy"org.json4s::json4s-jackson:4.0.5"
val sourcecode = ivy"com.lihaoyi::sourcecode:0.3.1"

object dependencies extends Module {
  object cde extends ScalaModule {
    def scalaVersion: T[String] = T(scala)
    def millSourcePath = super.millSourcePath / "cde"
  }

  object diplomacy extends Cross[DiplomacyCross](chisel)

  trait DiplomacyCross extends common.ChiselCrossModule {
    def scalaVersion: T[String] = T(scala)
    def millSourcePath = super.millSourcePath / "diplomacy"
    def moduleDeps = super.moduleDeps ++ Seq(cde)
    def ivyDeps = T(super.ivyDeps() ++ Agg(sourcecode))
  }
}

object rocketutils extends Cross[RocketutilsCross](chisel)

trait RocketutilsCross extends common.ChiselCrossModule {
  def scalaVersion = T(scala)

  def moduleDeps = super.moduleDeps ++ Seq(dependencies.cde, dependencies.diplomacy(crossValue))

  def ivyDeps = T(super.ivyDeps() ++ Agg(json4sJackson))
}
