// See LICENSE.SiFive for license details.

package freechips.rocketchip

import chisel3._

import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.nodes._

import freechips.rocketchip.prci.{HasClockDomainCrossing, HasResetDomainCrossing}

package object interrupts
{
  type IntInwardNode = InwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
  type IntNode = SimpleNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]

  type IntSyncInwardNode = InwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, SyncInterrupts]
  type IntSyncOutwardNode = OutwardNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, SyncInterrupts]
  type IntSyncNode = SimpleNodeHandle[IntSourcePortParameters, IntSinkPortParameters, IntEdge, SyncInterrupts]

  implicit class IntClockDomainCrossing(private val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: IntInwardNode) (implicit valName: ValName) = IntInwardClockCrossingHelper(valName.value, x, n)
    def crossOut(n: IntOutwardNode)(implicit valName: ValName) = IntOutwardClockCrossingHelper(valName.value, x, n)
    def cross(n: IntInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: IntOutwardNode)(implicit valName: ValName) = crossOut(n)
  }

  implicit class IntResetDomainCrossing(private val x: HasResetDomainCrossing) extends AnyVal {
    def crossIn (n: IntInwardNode) (implicit valName: ValName) = IntInwardResetCrossingHelper(valName.value, x, n)
    def crossOut(n: IntOutwardNode)(implicit valName: ValName) = IntOutwardResetCrossingHelper(valName.value, x, n)
    def cross(n: IntInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: IntOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
