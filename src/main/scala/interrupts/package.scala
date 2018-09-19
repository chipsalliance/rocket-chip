// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import freechips.rocketchip.diplomacy._

package object interrupts
{
  type IntInwardNode = InwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntNode = SimpleNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]

  implicit class IntClockDomainCrossing(val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: IntInwardNode) (implicit valName: ValName) = IntInwardCrossingHelper(valName.name, x, n)
    def crossOut(n: IntOutwardNode)(implicit valName: ValName) = IntOutwardCrossingHelper(valName.name, x, n)
    def cross(n: IntInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: IntOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
