// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class FrontBusParams(
  beatBytes: Int,
  blockBytes: Int,
  sbusCrossing: SubsystemClockCrossing = SynchronousCrossing(),
  sbusBuffer: BufferParams = BufferParams.none) extends HasTLBusParams

case object FrontBusKey extends Field[FrontBusParams]

class FrontBus(params: FrontBusParams)
              (implicit p: Parameters) extends TLBusWrapper(params, "front_bus")
    with HasTLXbarPhy {

  protected val sbus_xing = new CrossingHelper(this, params.sbusCrossing)
  def crossTLOut(implicit p: Parameters): TLNode  = sbus_xing.crossTLOut

  def fromPort[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLNameNode(name)): InwardNodeHandle[D,U,E,B] = {
    from("port" named name) { fixFrom(TLFIFOFixer.all, buffer) :=* gen }
  }

  def fromMasterNode
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: TLOutwardNode) {
    from("master" named name) { fixFrom(TLFIFOFixer.all, buffer) :=* gen }
  }

  def fromMaster[D,U,E,B <: Data]
      (name: Option[String] = None, buffer: BufferParams = BufferParams.none)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle] =
        TLNameNode(name)): InwardNodeHandle[D,U,E,B] = {
    from("master" named name) { fixFrom(TLFIFOFixer.all, buffer) :=* gen }
  }

  def fromCoherentChip(gen: => TLNode): TLInwardNode = {
    from("coherent_subsystem") { inwardNode :=* gen }
  }

  def toSystemBus(gen: => TLInwardNode) {
    to("sbus") { gen :=* TLBuffer(params.sbusBuffer) :=* outwardNode }
  }
}
