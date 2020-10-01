// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._

case class AHBSlaveMonitorArgs(edge: AHBEdgeParameters)

abstract class AHBSlaveMonitorBase(args: AHBSlaveMonitorArgs) extends Module
{
  val io = IO(new Bundle {
    val in = Input(new AHBSlaveBundle(args.edge.bundle))
  })

  def legalize(bundle: AHBSlaveBundle, edge: AHBEdgeParameters, reset: Reset): Unit
  legalize(io.in, args.edge, reset)
}


case class AHBMasterMonitorArgs(edge: AHBEdgeParameters)

abstract class AHBMasterMonitorBase(args: AHBMasterMonitorArgs) extends Module
{
  val io = IO(new Bundle {
    val in = Input(new AHBMasterBundle(args.edge.bundle))
  })

  def legalize(bundle: AHBMasterBundle, edge: AHBEdgeParameters, reset: Reset): Unit
  legalize(io.in, args.edge, reset)
}
