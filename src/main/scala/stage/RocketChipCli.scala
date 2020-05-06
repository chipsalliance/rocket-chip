// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import firrtl.options.Shell

import freechips.rocketchip.firrtl.StabilizeQueuesAspect

trait RocketChipCli { this: Shell =>

  parser.note("Rocket Chip Compiler Options")
  Seq(
    TopModuleAnnotation,
    ConfigsAnnotation,
    OutputBaseNameAnnotation,
    StabilizeQueuesAspect,
  )
    .foreach(_.addOptions(parser))

}
