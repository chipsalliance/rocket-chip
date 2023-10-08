import mill._
import mill.scalalib._

trait HasChisel
  extends ScalaModule {
  // Define these for building chisel from source
  def chiselModule: Option[ScalaModule]

  override def moduleDeps = super.moduleDeps ++ chiselModule

  def chiselPluginJar: T[Option[PathRef]]

  override def scalacOptions = T(super.scalacOptions() ++ chiselPluginJar().map(path => s"-Xplugin:${path.path}"))

  override def scalacPluginClasspath: T[Agg[PathRef]] = T(super.scalacPluginClasspath() ++ chiselPluginJar())

  // Define these for building chisel from ivy
  def chiselIvy: Option[Dep]

  override def ivyDeps = T(super.ivyDeps() ++ chiselIvy)

  def chiselPluginIvy: Option[Dep]

  override def scalacPluginIvyDeps: T[Agg[Dep]] = T(super.scalacPluginIvyDeps() ++ chiselPluginIvy.map(Agg(_)).getOrElse(Agg.empty[Dep]))
}

trait MacrosModule
  extends ScalaModule {

  def scalaReflectIvy: Dep

  override def ivyDeps = T(super.ivyDeps() ++ Some(scalaReflectIvy))
}


trait RocketChipModule
  extends HasChisel {
  override def mainClass = T(Some("freechips.rocketchip.diplomacy.Main"))

  def macrosModule: MacrosModule

  // should be hardfloat/common.sc#HardfloatModule
  def hardfloatModule: ScalaModule

  // should be cde/common.sc#CDEModule
  def cdeModule: ScalaModule

  def mainargsIvy: Dep

  def json4sJacksonIvy: Dep

  override def moduleDeps = super.moduleDeps ++ Seq(macrosModule, hardfloatModule, cdeModule)

  override def ivyDeps = T(
    super.ivyDeps() ++ Agg(
      mainargsIvy,
      json4sJacksonIvy
    )
  )
}
