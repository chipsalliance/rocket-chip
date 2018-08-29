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
  atomics: Option[BusAtomics] = Some(BusAtomics()),
  pbusBuffer: BufferParams = BufferParams.none,
  policy: TLArbiter.Policy = TLArbiter.roundRobin) extends HasTLBusParams

case object SystemBusKey extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "system_bus")
    with CanAttachTLSlaves
    with CanAttachTLMasters
    with HasTLXbarPhy {

  val cbus_params = new PeripheryBusParams(
    p(PeripheryBusKey).beatBytes,
    params.blockBytes,
    params.atomics,
    NoCrossing)
  val control_bus = LazyModule(new PeripheryBus(cbus_params))
  control_bus.crossFromSystemBus { this.toSlaveBus("cbus") }

  private val master_splitter = LazyModule(new TLSplitter)
  inwardNode :=* master_splitter.node

  def busView = master_splitter.node.edges.in.head

  def toSlaveBus(name: String): (=> TLInwardNode) => NoHandle =
    gen => to(s"bus_named_$name") {
      (gen
        :*= TLFIFOFixer(TLFIFOFixer.all)
        :*= TLWidthWidget(params.beatBytes)
        :*= TLBuffer(params.pbusBuffer)
        :*= outwardNode)
    }

  def fromMasterBus(name: String): (=> TLOutwardNode) => NoHandle =
    gen => from(s"bus_named_$name") { master_splitter.node :=* gen }

  def toMemoryBus(gen: => TLInwardNode) {
    to("mbus") { gen := outwardNode }
  }

  def toSplitSlave[D,U,E,B <: Data]
      (name: Option[String] = None)
      (gen: => NodeHandle[TLClientPortParameters,TLManagerPortParameters,TLEdgeIn,TLBundle,D,U,E,B] =
        TLNameNode(name)): OutwardNodeHandle[D,U,E,B] = {
    to("slave" named name) { gen :=* master_splitter.node }
  }

  def fromTile
      (name: Option[String], buffer: BufferParams = BufferParams.none, cork: Option[Boolean] = None)
      (gen: => TLOutwardNode): NoHandle = {
    from("tile" named name) {
      master_splitter.node :=* TLBuffer(buffer) :=* TLFIFOFixer(TLFIFOFixer.allUncacheable) :=* gen
    }
  }
}
