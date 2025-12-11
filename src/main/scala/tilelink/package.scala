// See LICENSE.SiFive for license details.

package freechips.rocketchip

import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.nodes._

import freechips.rocketchip.prci.{HasResetDomainCrossing, HasClockDomainCrossing}

package object tilelink
{
  type TLInwardNode = InwardNodeHandle[TLMasterPortParameters, TLSlavePortParameters, TLEdgeIn, TLBundle]
  type TLOutwardNode = OutwardNodeHandle[TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLBundle]
  type TLNode = NodeHandle[TLMasterPortParameters, TLSlavePortParameters, TLEdgeIn, TLBundle, TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLBundle]
  type TLManagerParameters = TLSlaveParameters
  type TLManagerPortParameters = TLSlavePortParameters
  type TLClientParameters = TLMasterParameters
  type TLClientPortParameters = TLMasterPortParameters

  implicit class TLClockDomainCrossing(private val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: TLInwardNode) (implicit valName: ValName) = TLInwardClockCrossingHelper (valName.value, x, n)
    def crossOut(n: TLOutwardNode)(implicit valName: ValName) = TLOutwardClockCrossingHelper(valName.value, x, n)
    def cross(n: TLInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: TLOutwardNode)(implicit valName: ValName) = crossOut(n)
  }

  implicit class TLResetDomainCrossing(private val x: HasResetDomainCrossing) extends AnyVal {
    def crossIn (n: TLInwardNode) (implicit valName: ValName) = TLInwardResetCrossingHelper (valName.value, x, n)
    def crossOut(n: TLOutwardNode)(implicit valName: ValName) = TLOutwardResetCrossingHelper(valName.value, x, n)
    def cross(n: TLInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: TLOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
