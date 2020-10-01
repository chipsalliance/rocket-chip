// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._

case class APBMonitorArgs(edge: APBEdgeParameters)

abstract class APBMonitorBase(args: APBMonitorArgs) extends Module
{
  val io = IO(new Bundle {
    val in = Input(new APBBundle(args.edge.bundle))
  })

  def legalize(bundle: APBBundle, edge: APBEdgeParameters, reset: Reset): Unit
  legalize(io.in, args.edge, reset)
}
