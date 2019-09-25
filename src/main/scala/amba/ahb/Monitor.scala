// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import chisel3.core.Reset
import freechips.rocketchip.config.Parameters

case class AHBMonitorArgs(edge: AHBEdgeParameters)

abstract class AHBMonitorBase(args: AHBMonitorArgs) extends Module
{
  val io = IO(new Bundle {
    val in = Input(new AHBSlaveBundle(args.edge.bundle))
  })

  def legalize(bundle: AHBSlaveBundle, edge: AHBEdgeParameters, reset: Reset): Unit
  legalize(io.in, args.edge, reset)
}
