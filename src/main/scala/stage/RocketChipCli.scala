// See LICENSE.SiFive for license details.

package freechips.rocketchip.stage

import firrtl.options.Shell

import freechips.rocketchip.firrtl.StabilizeNamesAspect

trait RocketChipCli { this: Shell =>

  parser.note("Rocket Chip Compiler Options")
  Seq(
    TopModuleAnnotation,
    ConfigsAnnotation,
    OutputBaseNameAnnotation,
    StabilizeNamesAspect
  )
    .foreach(_.addOptions(parser))

}
