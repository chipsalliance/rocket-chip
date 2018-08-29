// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import freechips.rocketchip.diplomacy._

package object tilelink
{
  type TLInwardNode = InwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLBundle]
  type TLOutwardNode = OutwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLBundle]
  type TLNode = NodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLBundle, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLBundle]

  implicit class TLClockDomainCrossing(val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: TLInwardNode) (implicit valName: ValName) = TLInwardCrossingHelper (valName.name, x, n)
    def crossOut(n: TLOutwardNode)(implicit valName: ValName) = TLOutwardCrossingHelper(valName.name, x, n)
    def cross(n: TLInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: TLOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
