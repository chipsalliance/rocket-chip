// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import chisel3._

case class AXI4MonitorArgs(edge: AXI4EdgeParameters)

abstract class AXI4MonitorBase(args: AXI4MonitorArgs) extends Module
{
  val io = IO(new Bundle {
    val in = Input(new AXI4Bundle(args.edge.bundle))
  })

  def legalize(bundle: AXI4Bundle, edge: AXI4EdgeParameters, reset: Reset): Unit
  legalize(io.in, args.edge, reset)
}
