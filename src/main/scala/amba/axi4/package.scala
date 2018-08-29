// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import Chisel._
import freechips.rocketchip.diplomacy._

package object axi4
{
  type AXI4Node = SimpleNodeHandle[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4Bundle]
  type AXI4OutwardNode = OutwardNodeHandle[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4Bundle]
  type AXI4InwardNode = InwardNodeHandle[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4Bundle]

  implicit class AXI4ClockDomainCrossing(val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: AXI4InwardNode) (implicit valName: ValName) = AXI4InwardCrossingHelper(valName.name, x, n)
    def crossOut(n: AXI4OutwardNode)(implicit valName: ValName) = AXI4OutwardCrossingHelper(valName.name, x, n)
    def cross(n: AXI4InwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: AXI4OutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
