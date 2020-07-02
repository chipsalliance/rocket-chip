// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import freechips.rocketchip.diplomacy._

package object tilelink
{
  type TLInwardNode = InwardNodeHandle[TLMasterPortParameters, TLSlavePortParameters, TLEdgeIn, TLBundle]
  type TLOutwardNode = OutwardNodeHandle[TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLBundle]
  type TLNode = NodeHandle[TLMasterPortParameters, TLSlavePortParameters, TLEdgeIn, TLBundle, TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLBundle]
  type TLMixedNodeCancel = NodeHandle[TLMasterPortParameters, TLSlavePortParameters, TLEdgeIn, TLBundle_ACancel, TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLBundle]
  type TLNode_ACancel    = NodeHandle[TLMasterPortParameters, TLSlavePortParameters, TLEdgeIn, TLBundle_ACancel, TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLBundle_ACancel]
  type TLManagerParameters = TLSlaveParameters
  type TLManagerPortParameters = TLSlavePortParameters
  type TLClientParameters = TLMasterParameters
  type TLClientPortParameters = TLMasterPortParameters

  implicit class TLClockDomainCrossing(val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: TLInwardNode) (implicit valName: ValName) = TLInwardCrossingHelper (valName.name, x, n)
    def crossOut(n: TLOutwardNode)(implicit valName: ValName) = TLOutwardCrossingHelper(valName.name, x, n)
    def cross(n: TLInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: TLOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
