// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class SystemBusParams(
  beatBytes: Int,
  blockBytes: Int,
  pbusBuffer: BufferParams = BufferParams.none,
  arithmeticAtomics: Boolean = true,
  bufferAtomics: BufferParams = BufferParams.default) extends HasTLBusParams

case object SystemBusKey extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "system_bus")
    with CanAttachTLSlaves
    with CanAttachTLMasters
    with HasTLXbarPhy {

  val cbus_params = new PeripheryBusParams(
    p(PeripheryBusKey).beatBytes,
    params.blockBytes,
    params.arithmeticAtomics,
    params.bufferAtomics,
    SynchronousCrossing())
  val control_bus = LazyModule(new PeripheryBus(cbus_params))
  control_bus.fromSystemBus {
    (TLFIFOFixer(TLFIFOFixer.all)
      :*= TLWidthWidget(params.beatBytes)
      :*= TLBuffer(params.pbusBuffer)
      :*= outwardNode)
  }

  private val master_splitter = LazyModule(new TLSplitter)
  inwardNode :=* master_splitter.node

  def busView = master_splitter.node.edges.in.head

  def toPeripheryBus(gen: => TLNode): TLOutwardNode = {
    to("pbus") {
      (gen
        :*= TLFIFOFixer(TLFIFOFixer.all)
        :*= TLWidthWidget(params.beatBytes)
        :*= TLBuffer(params.pbusBuffer)
        :*= outwardNode)
    }
  }

  def toMemoryBus(gen: => TLInwardNode) {
    to("mbus") { gen := outwardNode }
  }

  def toSplitSlave[D,U,E,B <: Data]
      (name: Option[String] = None)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :=* master_splitter.node }
  }

  def fromFrontBus(gen: => TLNode): TLInwardNode = {
    from("front_bus") { master_splitter.node :=* gen }
  }

  def fromTile
      (name: Option[String], buffer: BufferParams = BufferParams.none, cork: Option[Boolean] = None)
      (gen: => TLNode): TLInwardNode = {
    from("tile" named name) {
      master_splitter.node :=* TLBuffer(buffer) :=* TLFIFOFixer(TLFIFOFixer.allUncacheable) :=* gen
    }
  }
}
