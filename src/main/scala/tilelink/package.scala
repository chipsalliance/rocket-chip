// See LICENSE.SiFive for license details.

package freechips.rocketchip

import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.nodes._

import freechips.rocketchip.prci.{HasResetDomainCrossing, HasClockDomainCrossing}

package object tilelink
{
  type TLInwardNode = InwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLBundle]
  type TLOutwardNode = OutwardNodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLBundle]
  type TLNode = NodeHandle[TLClientPortParameters, TLManagerPortParameters, TLEdgeIn, TLBundle, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLBundle]

  implicit class TLClockDomainCrossing(private val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: TLInwardNode) (implicit valName: ValName) = TLInwardClockCrossingHelper (valName.name, x, n)
    def crossOut(n: TLOutwardNode)(implicit valName: ValName) = TLOutwardClockCrossingHelper(valName.name, x, n)
    def cross(n: TLInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: TLOutwardNode)(implicit valName: ValName) = crossOut(n)
  }

  implicit class TLResetDomainCrossing(private val x: HasResetDomainCrossing) extends AnyVal {
    def crossIn (n: TLInwardNode) (implicit valName: ValName) = TLInwardResetCrossingHelper (valName.name, x, n)
    def crossOut(n: TLOutwardNode)(implicit valName: ValName) = TLOutwardResetCrossingHelper(valName.name, x, n)
    def cross(n: TLInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: TLOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
