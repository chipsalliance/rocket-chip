// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import org.chipsalliance.diplomacy.ValName
import org.chipsalliance.diplomacy.nodes.{SimpleNodeHandle, OutwardNodeHandle, InwardNodeHandle}

import freechips.rocketchip.prci.{HasClockDomainCrossing, HasResetDomainCrossing}

/**
  * Provide bundles, adapters and devices etc for AMBA AXI4 protocol.
  */
package object axi4
{
  type AXI4Node = SimpleNodeHandle[AXI4ManagerPortParameters, AXI4SubordinatePortParameters, AXI4EdgeParameters, AXI4Bundle]
  type AXI4OutwardNode = OutwardNodeHandle[AXI4ManagerPortParameters, AXI4SubordinatePortParameters, AXI4EdgeParameters, AXI4Bundle]
  type AXI4InwardNode = InwardNodeHandle[AXI4ManagerPortParameters, AXI4SubordinatePortParameters, AXI4EdgeParameters, AXI4Bundle]

  implicit class AXI4ClockDomainCrossing(private val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: AXI4InwardNode) (implicit valName: ValName) = AXI4InwardClockCrossingHelper(valName.value, x, n)
    def crossOut(n: AXI4OutwardNode)(implicit valName: ValName) = AXI4OutwardClockCrossingHelper(valName.value, x, n)
    def cross(n: AXI4InwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: AXI4OutwardNode)(implicit valName: ValName) = crossOut(n)
  }

  implicit class AXI4ResetDomainCrossing(private val x: HasResetDomainCrossing) extends AnyVal {
    def crossIn (n: AXI4InwardNode) (implicit valName: ValName) = AXI4InwardResetCrossingHelper(valName.value, x, n)
    def crossOut(n: AXI4OutwardNode)(implicit valName: ValName) = AXI4OutwardResetCrossingHelper(valName.value, x, n)
    def cross(n: AXI4InwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: AXI4OutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
