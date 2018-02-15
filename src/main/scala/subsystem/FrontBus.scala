// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class FrontBusParams(beatBytes: Int, blockBytes: Int) extends HasTLBusParams

case object FrontBusKey extends Field[FrontBusParams]

class FrontBus(params: FrontBusParams, val crossing: SubsystemClockCrossing = SynchronousCrossing())
              (implicit p: Parameters) extends TLBusWrapper(params, "FrontBus")
    with HasTLXbarPhy
    with HasCrossing {

  def fromPort[D,U,E,B <: Data](
        name: Option[String] = None,
        buffers: Int = 1)
      (gen: => NodeHandle[D,U,E,B,TLClientPortParameters,TLManagerPortParameters,TLEdgeOut,TLBundle]): InwardNodeHandle[D,U,E,B] = {
    from(s"Port${name.getOrElse("")}") {
      val nodes = TLFIFOFixer(TLFIFOFixer.all) +: TLBuffer.chain(buffers)
      inwardNode :=* nodes.reduce(_ :=* _) :=* gen
    }
  }

  def fromMaster(name: Option[String] = None, buffers: Int = 1)
                (gen: => TLNode): TLInwardNode = {
    from(s"Master${name.getOrElse("")}") {
      inwardNode :=* TLBuffer.chain(buffers).reduce(_ :=* _) :=* gen
    }
  }

  def fromCoherentChip(gen: => TLNode): TLInwardNode = {
    from("CoherentChip") { inwardNode :=* gen }
  }

  def toSystemBus(buffer: BufferParams = BufferParams.none)
                 (gen: => TLInwardNode) {
    to("SystemBus") { gen :*= TLBuffer(buffer) :*= outwardNode }
  }
}

/** Provides buses that serve as attachment points,
  * for use in traits that connect individual devices or external ports.
  */
trait HasFrontBus extends HasSystemBus {
  private val frontbusParams = p(FrontBusKey)
  val frontbusBeatBytes = frontbusParams.beatBytes

  val fbus = LazyModule(new FrontBus(frontbusParams))

  FlipRendering { implicit p =>
    fbus.toSystemBus() { sbus.fromFrontBus { fbus.crossTLOut } }
  }
}
