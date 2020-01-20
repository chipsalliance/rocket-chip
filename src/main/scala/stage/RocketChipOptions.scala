// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import chisel3.experimental.BaseModule

import freechips.rocketchip.config.Config

class RocketChipOptions private[stage] (
  val config: Option[Class[_ <: Config]] = None,
  val topModule: Option[Class[_ <: Any]] = None ) {

  private[stage] def copy(
    config: Option[Class[_ <: Config]] = config,
    topModule: Option[Class[_ <: Any]] = topModule ): RocketChipOptions = {

    new RocketChipOptions(config=config, topModule=topModule)

  }

  lazy val longName: Option[String] = (config, topModule) match {
    case (Some(a), Some(b)) => Some(s"${a.getName}:${b.getName}")
    case _                  => None
  }

}
