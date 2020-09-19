// See LICENSE.SiFive for license details.

package freechips.rocketchip

import chisel3.{Bool, Vec}
import freechips.rocketchip.diplomacy.{HasClockDomainCrossing, _}
import freechips.rocketchip.prci.{HasResetDomainCrossing}

package object interrupts
{
  type IntInwardNode = InwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntNode = SimpleNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]

  implicit class IntClockDomainCrossing(private val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: IntInwardNode) (implicit valName: ValName) = IntInwardClockCrossingHelper(valName.name, x, n)
    def crossOut(n: IntOutwardNode)(implicit valName: ValName) = IntOutwardClockCrossingHelper(valName.name, x, n)
    def cross(n: IntInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: IntOutwardNode)(implicit valName: ValName) = crossOut(n)
  }

  implicit class IntResetDomainCrossing(private val x: HasResetDomainCrossing) extends AnyVal {
    def crossIn (n: IntInwardNode) (implicit valName: ValName) = IntInwardResetCrossingHelper(valName.name, x, n)
    def crossOut(n: IntOutwardNode)(implicit valName: ValName) = IntOutwardResetCrossingHelper(valName.name, x, n)
    def cross(n: IntInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: IntOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
