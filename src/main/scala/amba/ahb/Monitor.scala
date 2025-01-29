// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._

case class AHBSubordinateMonitorArgs(edge: AHBEdgeParameters)

abstract class AHBSubordinateMonitorBase(args: AHBSubordinateMonitorArgs) extends Module
{
  val io = IO(new Bundle {
    val in = Input(new AHBSubordinateBundle(args.edge.bundle))
  })

  def legalize(bundle: AHBSubordinateBundle, edge: AHBEdgeParameters, reset: Reset): Unit
  legalize(io.in, args.edge, reset)
}


case class AHBManagerMonitorArgs(edge: AHBEdgeParameters)

abstract class AHBManagerMonitorBase(args: AHBManagerMonitorArgs) extends Module
{
  val io = IO(new Bundle {
    val in = Input(new AHBManagerBundle(args.edge.bundle))
  })

  def legalize(bundle: AHBManagerBundle, edge: AHBEdgeParameters, reset: Reset): Unit
  legalize(io.in, args.edge, reset)
}
