// See LICENSE.SiFive for license details.

package freechips.rocketchip

import freechips.rocketchip.diplomacy._

package object prci
{
  type ClockInwardNode = InwardNodeHandle[ClockSourceParameters, ClockSinkParameters, ClockEdgeParameters, ClockBundle]
  type ClockOutwardNode = OutwardNodeHandle[ClockSourceParameters, ClockSinkParameters, ClockEdgeParameters, ClockBundle]
  type ClockNode = NodeHandle[ClockSourceParameters, ClockSinkParameters, ClockEdgeParameters, ClockBundle, ClockSourceParameters, ClockSinkParameters, ClockEdgeParameters, ClockBundle]
  def asyncMux[T](xType: ClockCrossingType, async: T, notasync: T): T = xType match {
    case _: AsynchronousCrossing => async
    case _ => notasync
  }
}
