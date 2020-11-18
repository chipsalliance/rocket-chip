// See LICENSE.SiFive for license details.

package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.BundleBridgeNexus.fillN
import freechips.rocketchip.util.{BlockDuringReset, Blockable}

object BundleBridgeBlockDuringReset {
  def apply[T <: Data : Blockable](
    resetCrossingType: ResetCrossingType,
    name: Option[String] = None,
    registered: Boolean = false,
    default: Option[() => T] = None,
    inputRequiresOutput: Boolean = false,
    shouldBeInlined: Boolean = true
  )(implicit p: Parameters): BundleBridgeNexusNode[T] = {
    val nexus = LazyModule(new BundleBridgeNexus[T](
      inputFn = (s: Seq[T]) => {
        val data = BundleBridgeNexus.requireOne[T](registered)(s)
        resetCrossingType match {
          case _: NoResetCrossing => data
          case s: StretchedResetCrossing => BlockDuringReset(data, s.cycles)
        }
      },
      outputFn = fillN[T](registered) _,
      default = default,
      inputRequiresOutput = inputRequiresOutput,
      shouldBeInlined = shouldBeInlined
    ))
    name.foreach(nexus.suggestName(_))
    nexus.node
  }
}
