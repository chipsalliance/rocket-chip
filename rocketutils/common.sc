import mill._
import mill.scalalib._

trait ChiselModule extends ScalaModule {
  def scalaVersion: T[String]
  def chiselVersion: T[String]

  def ivyDeps = T(super.ivyDeps() ++ Some(ivy"org.chipsalliance::chisel:${chiselVersion()}"))
  def scalacPluginIvyDeps = T(super.ivyDeps() ++ Agg(ivy"org.chipsalliance:::chisel-plugin:${chiselVersion()}"))
}

trait ChiselCrossModule extends ScalaModule with Cross.Module[String] {
  def scalaVersion: T[String]
  def chiselVersion: T[String] = T(crossValue)

  def ivyDeps = T(super.ivyDeps() ++ Some(ivy"org.chipsalliance::chisel:${chiselVersion()}"))
  def scalacPluginIvyDeps = T(super.ivyDeps() ++ Agg(ivy"org.chipsalliance:::chisel-plugin:${chiselVersion()}"))
}

trait ChiselSourceModule extends ScalaModule {
  def scalaVersion: T[String]

  def chiselModule: ScalaModule
  def chiselPluginJar: T[PathRef]

  def moduleDeps = super.moduleDeps ++ Agg(chiselModule)
  def scalacOptions = T(super.scalacOptions() ++ Seq(s"-Xplugin:${chiselPluginJar().path}"))
  def scalacPluginClasspath: T[Agg[PathRef]] = T(super.scalacPluginClasspath() ++ Agg(chiselPluginJar()))
}
