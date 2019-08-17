package freechips.rocketchip.system

import pureconfig.generic.auto._

final case class MySubsystemConfig(
  base: Int,
  size: Int,
  enableRocket: Boolean
)

final case class MySystemConfig(
  language: String
)

final case class MyConfig(
  subsystem: MySubsystemConfig,
  system: MySystemConfig
)

object Top {

  // Load JSON config from application.conf
  val cfg = pureconfig.loadConfig[MyConfig]

  // Parse params to local variables
  val base: Int = cfg.right.get.subsystem.base
  val size: Int = cfg.right.get.subsystem.size

}
